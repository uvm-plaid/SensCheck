{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module DpMinst where

-- This is the differentially private version of the mnist example from Grenade:
-- https://github.com/HuwCampbell/grenade/blob/master/examples/main/mnist.hs

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Except
import Data.Attoparsec.Text qualified as A
import Data.Foldable (fold)
import Data.Kind (Constraint)
import Data.List (foldl')
import Data.List.Singletons (Tail)
import Data.Proxy (Proxy (Proxy))
import Data.Singletons.Decide
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Storable qualified as V
import Distance (Distance)
import Distance qualified
import GHC.Base (Type)
import GHC.Generics ((:+:) (L1))
import GHC.TypeLits
import GHC.TypeLits qualified as TL
import Grenade
import Grenade.Core.Shape (Sing (..))
import Grenade.Utils.OneHot
import Numeric.LinearAlgebra (Normed (norm_2), maxIndex)
import Numeric.LinearAlgebra qualified as LA
import Numeric.LinearAlgebra qualified as Matrix
import Numeric.LinearAlgebra.Data qualified as SAD
import Numeric.LinearAlgebra.Static (R (..), Sized (unwrap), (#))
import Numeric.LinearAlgebra.Static qualified as SA
import Options.Applicative
import Prelude.Singletons (Head, Last, SingI (..))
import Primitives qualified as Solo
import Privacy qualified as Solo
import Sensitivity (SDouble (D_UNSAFE), SList (SList_UNSAFE))
import Sensitivity qualified as Solo
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Positive (Positive))
import Test.QuickCheck.Gen (oneof)
import Prelude

type FL i o =
  Network
    '[FullyConnected i o, Logit]
    '[ 'D1 i, 'D1 o, 'D1 o]

-- More simple layer to work on
type TestLayer = '[FullyConnected 1 1]

-- Example with nested gradients
type TestLayer2 = '[InceptionMini 28 28 1 5 9]

-- The definition of our convolutional neural network.
type MNIST =
  Network Layers Shapes'

-- Layers in network
type Layers =
  '[ Reshape
   , Concat ('D3 28 28 1) Trivial ('D3 28 28 14) (InceptionMini 28 28 1 5 9)
   , Pooling 2 2 2 2
   , Relu
   , Concat ('D3 14 14 3) (Convolution 15 3 1 1 1 1) ('D3 14 14 15) (InceptionMini 14 14 15 5 10)
   , Crop 1 1 1 1
   , Pooling 3 3 3 3
   , Relu
   , Reshape
   , FL 288 80
   , FL 80 10
   ]

type Shapes' =
  '[ 'D2 28 28, 'D3 28 28 1,
      'D3 28 28 15, 'D3 14 14 15, 'D3 14 14 15, 'D3 14 14 18,
      'D3 12 12 18, 'D3 4 4 18, 'D3 4 4 18,
      'D1 288, 'D1 80, 'D1 10 ]

type Layers2 = '[Reshape, FL 10 2]
type Shapes2' = '[ 'D2 2 5, 'D1 10, 'D1 2]

randomMnist :: (MonadRandom m) => m MNIST
randomMnist = randomNetwork

-- Test shape
type TestShapes' =
  '[ 'D2 1 1]

type LastShape = Last Shapes'

convTestDP ::
  forall e iterations s layers shapes len.
  (TL.KnownNat iterations, SingI (Last shapes), KnownNat len, FlattenGrads layers len) =>
  [LabeledInput shapes] ->
  Network layers shapes ->
  LearningParameters ->
  Solo.PM (Solo.ScalePriv (Solo.TruncatePriv e Solo.Zero (s Solo.+++ s)) iterations) (Network layers shapes)
convTestDP trainData initialNetwork rate = Solo.seqloop @iterations (runIteration trainData) initialNetwork
 where
  runIteration trainRows i net = trainDP @e @s @layers @shapes @len (rate{learningRate = learningRate rate * 0.9 ^ i}) net trainRows

-- training input and label (output)
type LabeledInput shapes = (S (Head shapes), S (Last shapes))

-- Update a network with new weights after training with an instance.
trainDP ::
  forall e s layers shapes len.
  (SingI (Last shapes), KnownNat len, FlattenGrads layers len) =>
  LearningParameters ->
  Network layers shapes ->
  [LabeledInput shapes] ->
  Solo.PM (Solo.TruncatePriv e Solo.Zero (s Solo.+++ s)) (Network layers shapes)
trainDP rate network trainRows = Solo.do
  let sensitiveTrainRows = Solo.SList_UNSAFE @Solo.L1 @_ @s $ STRAINROW_UNSAFE @Solo.Disc @shapes <$> trainRows
      gradSum = clippedGrad @len sensitiveTrainRows network
  noisyGrad <- laplaceGradients @e gradSum
  -- return $ applyUpdate rate network (noisyGrad / (length trainRows)) -- TODO this expects Gradients not a single gradient
  Solo.return $ applyUpdate rate network noisyGrad -- TODO divide by length trainRows

newtype SGradients (m :: Solo.CMetric) len (s :: Solo.SEnv) = SGRAD2_UNSAFE {unSGrad2 :: R len} deriving (Show)

instance (KnownNat h) => Distance (SGradients Solo.L2 h senv) where
  distance (SGRAD2_UNSAFE l) (SGRAD2_UNSAFE l2) = Distance.l2dist (D_UNSAFE @Solo.Diff <$> SAD.toList (SA.unwrap l)) (D_UNSAFE @Solo.Diff <$> SAD.toList (SA.unwrap l2))

laplaceGradients :: forall e s len layers. SGradients Solo.L2 len s -> Solo.PM (Solo.TruncatePriv e Solo.Zero s) (Gradients layers)
laplaceGradients gradient = undefined

-- The training row
newtype STrainRow (m :: Solo.NMetric) (shapes :: [Shape]) (s :: Solo.SEnv) = STRAINROW_UNSAFE {unSTrainRow :: LabeledInput shapes} deriving (Show)

instance Distance (STrainRow Solo.Disc shapes senv) where
  distance a b =
    let rowA = unSTrainRow a
        rowB = unSTrainRow b
        inputsAreEq = strainEq (fst rowA) (fst rowB)
        labelsAreEq = strainEq (snd rowA) (snd rowB)
     in if inputsAreEq && labelsAreEq then 0 else 1

strainEq :: S shape -> S shape -> Bool
strainEq (S1D x) (S1D y) = unwrap x == unwrap y
strainEq (S2D x) (S2D y) = unwrap x == unwrap y
strainEq (S3D x) (S3D y) = unwrap x == unwrap y

instance (SingI (Head shapes), SingI (Last shapes)) => Arbitrary (STrainRow m shapes s) where
  arbitrary = do
    input <- randomOfShapeGen
    label <- randomOfShapeGen
    pure $ STRAINROW_UNSAFE (input, label)

-- Takes a Shape. Get's the Matrix dimension. Then generates a List of the specified size.
randomOfShapeGen :: forall s. (SingI s) => Gen (S s)
randomOfShapeGen = case (sing :: Sing s) of
  D1Sing @x ->
    let size = fromIntegral $ TL.natVal @x Proxy
     in S1D . SA.fromList <$> replicateM size arbitrary
  D2Sing @x @y ->
    let size = fromIntegral $ TL.natVal @x Proxy * TL.natVal @y Proxy
     in S2D . SA.fromList <$> replicateM size arbitrary
  D3Sing @x @y @z ->
    let size = fromIntegral $ TL.natVal @x Proxy * TL.natVal @y Proxy * TL.natVal @z Proxy
     in S3D . SA.fromList <$> replicateM size arbitrary


-- Takes training examples and computes clipped gradients
clippedGrad ::
  forall len senv layers shapes.
  (KnownNat len, SingI (Last shapes), FlattenGrads layers len) =>
  Solo.SList Solo.L1 (STrainRow Solo.Disc shapes) senv ->
  Network layers shapes ->
  SGradients Solo.L2 len (senv Solo.+++ senv)
clippedGrad trainRows network =
  -- For every training example, backpropagate and clip the gradients
  let grads = oneGrad . unSTrainRow <$> Solo.unSList trainRows
      clipAmount = 1.0
      flattenedGrads = (\v -> l2clipVector (flattenGrads v) clipAmount) <$> grads
      summedGrads =  foldr (+) (SA.konst 0) flattenedGrads
   in SGRAD2_UNSAFE summedGrads
 where
  -- Takes single training example and calculates the gradient for that training example
  oneGrad (example, label) = backPropagate network example label

-- Flatten gradients into a single vector
class FlattenGrad grad len | grad -> len where
  flattenGrad :: grad -> R len
  unflattenGrad :: R len -> grad

class FlattenGrads layers len | layers -> len where
  flattenGrads :: Gradients layers -> R len
  unflattenGrads :: R len -> Gradients layers

emptyVector :: R 0
emptyVector = SA.konst 0

instance FlattenGrads '[] 0 where
  flattenGrads GNil = emptyVector
  unflattenGrads = undefined

instance (FlattenGrads layerTail lenTail, FlattenGrad (Gradient layer) lenHead, len ~ (lenHead + lenTail), KnownNat lenHead, KnownNat lenTail) => FlattenGrads (layer : layerTail) len where
  flattenGrads (grad :/> gradT) = flattenGrad grad # flattenGrads gradT
  unflattenGrads = undefined

heads :: [Gradients (layer : layerTail)] -> [Gradient layer]
heads ((grad1 :/> _) : t) = grad1 : heads t
heads [] = []

tails :: [Gradients (layer ': layerTail)] -> [Gradients layerTail]
tails ((_ :/> gradt) : t) = gradt : tails t

instance FlattenGrad (Gradients '[]) 0 where
  flattenGrad GNil = emptyVector
  unflattenGrad = undefined

instance (FlattenGrad (Gradient layer) headLen, FlattenGrad (Gradients layerTail) tailLen, len ~ (headLen + tailLen), KnownNat tailLen, KnownNat headLen) => FlattenGrad (Gradients (layer : layerTail)) len where
  flattenGrad (grad :/> gradt) = flattenGrad grad # flattenGrad gradt
  unflattenGrad = undefined

instance (KnownNat i, KnownNat o, KnownNat (o * i), KnownNat n, n ~ o + (o * i)) => FlattenGrad (FullyConnected' i o) n where
  flattenGrad (FullyConnected' wB wN) = wB # flattenMatrix wN
  unflattenGrad = undefined

flattenMatrix :: (KnownNat i, KnownNat o, KnownNat (i * o)) => SA.L i o -> R (i * o)
flattenMatrix = SA.fromList . concat . SAD.toLists . SA.unwrap

instance
  ( KnownNat channels
  , KnownNat filters
  , KnownNat kernelRows
  , KnownNat kernelColumns
  , KnownNat strideRows
  , KnownNat strideColumns
  , len ~ (kernelRows * kernelColumns * channels * filters)
  , KnownNat len
  ) =>
  FlattenGrad (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns) len
  where
  flattenGrad (Convolution' grad) = flattenMatrix grad
  unflattenGrad = undefined

instance (FlattenGrad leftGrad leftLen, FlattenGrad rightGrad rightLen, KnownNat leftLen, KnownNat rightLen, len ~ (leftLen + rightLen)) => FlattenGrad (Concat m leftGrad n rightGrad) len where
  flattenGrad (Concat g1 g2) = flattenGrad g1 # flattenGrad g2
  unflattenGrad = undefined

instance (FlattenGrad leftGrad leftLen, FlattenGrad rightGrad rightLen, KnownNat leftLen, KnownNat rightLen, len ~ (leftLen + rightLen)) => FlattenGrad (leftGrad, rightGrad) len where
  flattenGrad (g1, g2) = flattenGrad g1 # flattenGrad g2
  unflattenGrad = undefined

instance FlattenGrad () 0 where
  flattenGrad () = emptyVector
  unflattenGrad = undefined

type ClipAmount = Double

l2clipMatrix :: (KnownNat n, KnownNat m) => SA.L n m -> ClipAmount -> SA.L n m
l2clipMatrix m clipAmount =
  let norm = norm_2 m
   in if norm > clipAmount
        then SA.dmmap (\elem -> clipAmount * (elem / norm)) m
        else m

l2clipVector :: (KnownNat n) => SA.R n -> ClipAmount -> SA.R n
l2clipVector v clipAmount =
  let norm = norm_2 v
   in if norm > clipAmount
        then SA.dvmap (\elem -> clipAmount * (elem / norm)) v
        else v

-- CLI stuff
data MnistOpts = MnistOpts FilePath FilePath Int LearningParameters

mnist' :: Parser MnistOpts
mnist' =
  MnistOpts
    <$> argument str (metavar "TRAIN")
    <*> argument str (metavar "VALIDATE")
    <*> option auto (long "iterations" <> short 'i' <> value 15)
    <*> ( LearningParameters
            <$> option auto (long "train_rate" <> short 'r' <> value 0.01)
            <*> option auto (long "momentum" <> value 0.9)
            <*> option auto (long "l2" <> value 0.0005)
        )

-- main :: IO ()
-- main = do
--   MnistOpts mnist vali iter rate <- execParser (info (mnist' <**> helper) idm)
--   putStrLn "Training convolutional neural network..."

--   res <- runExceptT $ convTest iter mnist vali rate
--   case res of
--     Right () -> pure ()
--     Left err -> putStrLn err

readMNIST :: FilePath -> ExceptT String IO [(S ('D2 28 28), S ('D1 10))]
readMNIST mnist = ExceptT $ do
  mnistdata <- T.readFile mnist
  return $ traverse (A.parseOnly parseMNIST) (T.lines mnistdata)

parseMNIST :: A.Parser (S ('D2 28 28), S ('D1 10))
parseMNIST = do
  Just lab <- oneHot <$> A.decimal
  pixels <- many (A.char ',' >> A.double)
  image <- maybe (fail "Parsed row was of an incorrect size") pure (fromStorable . V.fromList $ pixels)
  return (image, lab)


-- convTest :: Int -> FilePath -> FilePath -> LearningParameters -> ExceptT String IO ()
-- convTest iterations trainFile validateFile rate = do
--   net0 <- lift randomMnist
--   trainData <- readMNIST trainFile
--   validateData <- readMNIST validateFile
--   lift $ foldM_ (runIteration trainData validateData) net0 [1 .. iterations]
--  where
--   -- TODO train calls applyUpdate and backPropagate
--   -- Goal: Override those functions
--   trainEach rate' network (i, o) = train rate' network i o

--   runIteration trainRows validateRows net i = do
--     let trained' = foldl' (trainEach (rate{learningRate = learningRate rate * 0.9 ^ i})) net trainRows
--     let res = fmap (\(rowP, rowL) -> (rowL,) $ runNet trained' rowP) validateRows
--     let res' = fmap (\(S1D label, S1D prediction) -> (maxIndex (SA.extract label), maxIndex (SA.extract prediction))) res
--     print trained'
--     putStrLn $ "Iteration " ++ show i ++ ": " ++ show (length (filter ((==) <$> fst <*> snd) res')) ++ " of " ++ show (length res')
--     return trained'

-- Useful for generating lists of the same size with quickcheck
data SameSizedSLists m t senv = SameSizedSLists (Solo.SList m t senv) (Solo.SList m t senv) deriving (Show)

-- Have quickcheck generate lists of the same size

instance (Arbitrary (t senv)) => Arbitrary (SameSizedSLists m t senv) where
  arbitrary = do
    l1 <- replicateM 1 arbitrary
    l2 <- replicateM 1 arbitrary
    pure $ SameSizedSLists (SList_UNSAFE l1) (SList_UNSAFE l2)

-- Some unused code that was useful for testing

trainingExample :: Solo.SList Solo.L1 (STrainRow Solo.Disc Shapes') senv
trainingExample =
  let input = S2D $ SA.fromList [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      label = S1D $ SA.fromList [1, 0] in
      SList_UNSAFE [STRAINROW_UNSAFE (input, label)]


trainingExample2 :: Solo.SList Solo.L1 (STrainRow Solo.Disc Shapes') senv
trainingExample2 =
  let input = S2D $ SA.fromList [1, 1, 1, 1, 1, 1, 1, 1, 1, 20000]
      label = S1D $ SA.fromList [1, 0] in
      SList_UNSAFE [STRAINROW_UNSAFE (input, label)]

staticNetwork num = Reshape :~> (FullyConnected (FullyConnected' (SA.konst num) (SA.konst num)) (FullyConnected' (SA.konst num) (SA.konst num)) :~> Logit :~> NNil) :~> NNil

-- Takes a Shape. Get's the Matrix dimension. Then generates a List of the specified size. But zeroed out.
zeroedOfShape :: forall s. (SingI s) => S s
zeroedOfShape = case (sing :: Sing s) of
  D1Sing @x ->
    let size = fromIntegral $ TL.natVal @x Proxy
     in S1D . SA.fromList $ replicate size 0
  D2Sing @x @y ->
    let size = fromIntegral $ TL.natVal @x Proxy * TL.natVal @y Proxy
     in S2D . SA.fromList $ replicate size 0
  D3Sing @x @y @z ->
    let size = fromIntegral $ TL.natVal @x Proxy * TL.natVal @y Proxy * TL.natVal @z Proxy
     in S3D . SA.fromList $ replicate size 0
