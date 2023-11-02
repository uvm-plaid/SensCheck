{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
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
import Sensitivity qualified as Solo
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
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

-- The shape
type Shapes' =
  '[ 'D2 28 28
   , 'D3 28 28 1
   , 'D3 28 28 15
   , 'D3 14 14 15
   , 'D3 14 14 15
   , 'D3 14 14 18
   , 'D3 12 12 18
   , 'D3 4 4 18
   , 'D3 4 4 18
   , 'D1 288
   , 'D1 80
   , 'D1 10
   ]

-- Test shape
type TestShapes' =
  '[ 'D2 1 1]

type LastShape = Last Shapes'

convTestDP ::
  forall e iterations s layers shapes.
  (TL.KnownNat iterations) =>
  [LabeledInput Shapes'] ->
  Network Layers Shapes' ->
  LearningParameters ->
  Solo.PM (Solo.ScalePriv (Solo.TruncatePriv e Solo.Zero s) iterations) (Network Layers Shapes') -- ExceptT String IO ()
convTestDP trainData initialNetwork rate = Solo.seqloop @iterations (runIteration trainData) initialNetwork
 where
  runIteration trainRows i net = do
    let trained' = trainDP @e @s (rate{learningRate = learningRate rate * 0.9 ^ i}) net trainRows
    trained'

-- training input and label (output)
type LabeledInput shapes = (S (Head shapes), S (Last shapes))

-- Update a network with new weights after training with an instance.
trainDP ::
  forall e s layers shapes.
  SingI (Last Shapes') =>
  LearningParameters ->
  Network Layers Shapes' ->
  [LabeledInput Shapes'] -> -- Should we be taking in the training data as sensitive list here? or expect the caller to provide it?
  Solo.PM (Solo.TruncatePriv e Solo.Zero s) (Network Layers Shapes')
trainDP rate network trainRows = Solo.do
  let sensitiveTrainRows = Solo.SList_UNSAFE @Solo.L2 @_ @s $ STRAINROW_UNSAFE @Solo.Disc @Shapes' <$> trainRows
      gradSum = clippedGrad sensitiveTrainRows network
  noisyGrad <- laplaceGradients @e @s gradSum
  -- return $ applyUpdate rate network (noisyGrad / (length trainRows)) -- TODO this expects Gradients not a single gradient
  Solo.return $ applyUpdate rate network noisyGrad -- TODO divide by length trainRows

newtype SGradients (m :: Solo.CMetric) (grads :: [Type]) (s :: Solo.SEnv) = SGRAD_UNSAFE {unSGrad :: Gradients grads}
newtype SGradients2 (m :: Solo.CMetric) len (s :: Solo.SEnv) = SGRAD2_UNSAFE {unSGrad2 :: R len}

instance (KnownNat h) => Distance (SGradients2 Solo.L2 h senv) where
  distance (SGRAD2_UNSAFE l) (SGRAD2_UNSAFE l2) = Distance.l2dist (SGrad <$> SAD.toList (SA.unwrap l)) (SGrad <$> SAD.toList (SA.unwrap l2))

newtype SGrad = SGrad Double deriving (Show, Eq)

instance Distance SGrad where
  distance (SGrad d1) (SGrad d2) = Distance.discdist d1 d2 -- TODO which metric abs or disc?

-- SHould return a non-sensitive Gradient
laplaceGradients :: forall e s. SGradients Solo.L2 Layers s -> Solo.PM (Solo.TruncatePriv e Solo.Zero s) (Gradients Layers)
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
randomOfShapeGen = do
  case (sing :: Sing s) of
    D1Sing @x ->
      let size = fromIntegral $ TL.natVal @x Proxy
       in S1D . SA.fromList <$> replicateM size arbitrary
    D2Sing @x @y ->
      let size = fromIntegral $ TL.natVal @x Proxy * TL.natVal @y Proxy
       in S2D . SA.fromList <$> replicateM size arbitrary
    D3Sing @x @y @z ->
      let size = fromIntegral $ TL.natVal @x Proxy * TL.natVal @y Proxy * TL.natVal @z Proxy
       in S3D . SA.fromList <$> replicateM size arbitrary

-- We are going to take a list of gradients and turn it into a single gradient
-- Takes all the training examples and computes gradients
-- Clips it
clippedGrad ::
  forall senv.
  Solo.SList Solo.L2 (STrainRow Solo.Disc Shapes') senv ->
  Network Layers Shapes' ->
  SGradients Solo.L2 Layers senv
clippedGrad trainRows network =
  -- For every training example, backpropagate and clip the gradients
  let grads = oneGrad . unSTrainRow <$> Solo.unSList trainRows
      clipAmount = 1.0
      -- sum the gradients, column-wise, to get 1-sensitive val
      gradSum = sumListOfGrads clipAmount grads
   in SGRAD_UNSAFE @Solo.L2 @_ @senv gradSum
 where
  -- Takes single training example and calculates the gradient for that training example
  oneGrad (example, label) = backPropagate network example label

-- We are going to take a list of gradients and turn it into a single gradient
-- Takes all the training examples and computes gradients
-- Clips it
clippedGrad2 ::
  forall len senv. -- TODO shapes layers.
  (KnownNat len, SingI (Last Shapes'), FlattenGrads Layers len) =>
  Solo.SList Solo.L2 (STrainRow Solo.Disc Shapes') senv ->
  Network Layers Shapes' ->
  SGradients2 Solo.L2 len senv
clippedGrad2 trainRows network =
  -- For every training example, backpropagate and clip the gradients
  let grads = oneGrad . unSTrainRow <$> Solo.unSList trainRows
      clipAmount = 1.0
      -- TODO Is the initial vector being all 0s ok?
      clippedGrad = l2clipVector (foldr (+) (SA.konst 0) $ flattenGrads <$> grads) clipAmount
   in SGRAD2_UNSAFE clippedGrad
 where
  -- Takes single training example and calculates the gradient for that training example
  oneGrad (example, label) = backPropagate network example label

-- testClippedGrad = clippedGrad2 @_ @TestShapes' @TestLayer2

-- We will use this in distance metric for the whole gradient thing
class FlattenGrad grad len | grad -> len where
  flattenGrad :: grad -> R len
  unflattenGrad :: R len -> grad

-- SumListOfGrads is a type class with 2 instances.
-- We need to take a regular list of Gradients which is a type level list.
-- And output a *single* list of type level Gradients.
--
-- e.g. [ '[FullyConnected, Pad] ] to a '[FullyConnect, Pad]
--
-- Also we will just take the Gradient of each layer and add them.
-- Well to combine things you can use a Monoid and also a fold.
--
-- > fold [Gradient FullyConnected, Gradient FullyConnected]
-- Gradient FullyConnected
--
-- The first instance takes in a empty type level list of gradients.
-- Result is a
--
-- Credit to https://www.morrowm.com/ for helping me with this technique.

class SumListOfClippedGrads layers where
  sumListOfGrads :: ClipAmount -> [Gradients layers] -> Gradients layers

instance SumListOfClippedGrads '[] where
  sumListOfGrads clipAmount _ = GNil

instance (Monoid (Gradient layer), ClipGrad (Gradient layer), UpdateLayer layer, SumListOfClippedGrads layerTail) => SumListOfClippedGrads (layer : layerTail) where
  sumListOfGrads clipAmount gradsl =
    let headsGrad = heads gradsl
        tailsGrad = tails gradsl
        -- Collapse list of Gradient layer and then clip
        (clippedGrad :: Gradient layer) = foldMap (l2clipGrad clipAmount) headsGrad
     in clippedGrad :/> sumListOfGrads clipAmount tailsGrad

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

-- Recursive case
instance (Distance (Gradient layer), UpdateLayer layer, Distance (SGradients Solo.L2 layerTail senv)) => Distance (SGradients Solo.L2 (layer : layerTail) senv) where
  distance _ _ = undefined

-- Base case
instance Distance (SGradients Solo.L2 '[] senv) where
  distance _ _ = undefined

heads :: [Gradients (layer : layerTail)] -> [Gradient layer]
heads ((grad1 :/> _) : t) = grad1 : heads t
heads [] = []

-- Ditto but tail
tails :: [Gradients (layer ': layerTail)] -> [Gradients layerTail]
tails ((_ :/> gradt) : t) = gradt : tails t

instance Semigroup (Gradients '[]) where
  GNil <> GNil = GNil

instance Monoid (Gradients '[]) where
  mempty = GNil

instance FlattenGrad (Gradients '[]) 0 where
  flattenGrad GNil = emptyVector
  unflattenGrad = undefined

-- The below instances handle nesting of Gradients in Gradients.
-- e.g. InceptionMini has Gradients in it
instance (Semigroup (Gradient layer), Semigroup (Gradients layerTail)) => Semigroup (Gradients (layer : layerTail)) where
  (grad1 :/> grad1t) <> (grad2 :/> grad2t) = (grad1 <> grad2) :/> (grad1t <> grad2t)

instance (Monoid (Gradient layer), Monoid (Gradients layerTail), UpdateLayer layer) => Monoid (Gradients (layer : layerTail)) where
  -- TODO not sure about this one
  mempty = mempty :/> mempty

instance (FlattenGrad (Gradient layer) headLen, FlattenGrad (Gradients layerTail) tailLen, len ~ (headLen + tailLen), KnownNat tailLen, KnownNat headLen) => FlattenGrad (Gradients (layer : layerTail)) len where
  flattenGrad (grad :/> gradt) = flattenGrad grad # flattenGrad gradt
  unflattenGrad = undefined

instance (Distance (Gradient layer), Distance (Gradients layerTail)) => Distance (Gradients (layer : layerTail)) where
  distance (grad1 :/> grad1t) (grad2 :/> grad2t) = undefined

instance (KnownNat i, KnownNat o, KnownNat (o * i), n ~ o + (o * i)) => FlattenGrad (FullyConnected' i o) n where
  flattenGrad (FullyConnected' wB wN) = wB # flattenMatrix wN
  unflattenGrad = undefined

flattenMatrix :: (KnownNat i, KnownNat o, KnownNat (i * o)) => SA.L i o -> R (i * o)
flattenMatrix = SA.fromList . concat . SAD.toLists . SA.unwrap

-- Gradient Layer Implementation
instance (KnownNat i, KnownNat o) => Semigroup (FullyConnected' i o) where
  (FullyConnected' wB wN) <> (FullyConnected' wB2 wN2) = FullyConnected' (wB + wB2) (wN + wN2)

instance (KnownNat i, KnownNat o) => Distance (FullyConnected' i o) where
  distance (FullyConnected' wB wN) (FullyConnected' wB2 wN2) = undefined

instance (KnownNat i, KnownNat o) => Monoid (FullyConnected' i o) where
  mempty = FullyConnected' (SA.vector [0 .. 0]) (SA.matrix [0 .. 0])

instance
  ( KnownNat channels
  , KnownNat filters
  , KnownNat kernelRows
  , KnownNat kernelColumns
  , KnownNat strideRows
  , KnownNat strideColumns
  ) =>
  Semigroup (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns)
  where
  (Convolution' grad1) <> (Convolution' grad2) = Convolution' (grad1 + grad2)

instance
  ( KnownNat channels
  , KnownNat filters
  , KnownNat kernelRows
  , KnownNat kernelColumns
  , KnownNat strideRows
  , KnownNat strideColumns
  , KnownNat (kernelRows * kernelColumns * channels)
  ) =>
  Monoid (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns)
  where
  mempty = Convolution' $ SA.matrix @(kernelRows * kernelColumns * channels) [0 .. 0]

instance
  ( KnownNat channels
  , KnownNat filters
  , KnownNat kernelRows
  , KnownNat kernelColumns
  , KnownNat strideRows
  , KnownNat strideColumns
  ) =>
  Distance (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns)
  where
  distance (Convolution' grad1) (Convolution' grad2) = undefined

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

instance (Semigroup x, Semigroup y) => Semigroup (Concat m x n y) where
  (Concat x1 y1) <> (Concat x2 y2) = Concat (x1 <> x2) (y1 <> y2)

instance (Monoid x, Monoid y) => Monoid (Concat m x n y) where
  mempty = Concat mempty mempty

instance (Distance x, Distance y) => Distance (Concat m x n y) where
  distance _ _ = undefined

instance (FlattenGrad leftGrad leftLen, FlattenGrad rightGrad rightLen, KnownNat leftLen, KnownNat rightLen, len ~ (leftLen + rightLen)) => FlattenGrad (Concat m leftGrad n rightGrad) len where
  flattenGrad (Concat g1 g2) = flattenGrad g1 # flattenGrad g2
  unflattenGrad = undefined

instance (FlattenGrad leftGrad leftLen, FlattenGrad rightGrad rightLen, KnownNat leftLen, KnownNat rightLen, len ~ (leftLen + rightLen)) => FlattenGrad (leftGrad, rightGrad) len where
  flattenGrad (g1, g2) = flattenGrad g1 # flattenGrad g2
  unflattenGrad = undefined

-- Hmm this might be bad maybe don't do the Gradient trick or provide the L1/L2 thing?
instance Distance () where
  distance () () = undefined

instance FlattenGrad () 0 where
  flattenGrad () = emptyVector
  unflattenGrad = undefined

type ClipAmount = Double

-- Clips gradients
-- https://programming-dp.com/ch12.html?highlight=clipping#gradient-clipping
class ClipGrad grad where
  l2clipGrad :: ClipAmount -> grad -> grad

instance
  ( KnownNat channels
  , KnownNat filters
  , KnownNat kernelRows
  , KnownNat kernelColumns
  , KnownNat strideRows
  , KnownNat strideColumns
  , KnownNat (kernelRows * kernelColumns * channels)
  ) =>
  ClipGrad (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns)
  where
  l2clipGrad clipAmount (Convolution' grad) = Convolution' $ l2clipMatrix grad clipAmount

l2clipMatrix :: (KnownNat n, KnownNat m) => SA.L n m -> ClipAmount -> SA.L n m
l2clipMatrix m clipAmount =
  let norm = norm_2 m
   in if norm > clipAmount
        then SA.dmmap (\elem -> clipAmount * (elem / norm)) m
        else m

l2clipVector :: KnownNat n => SA.R n -> ClipAmount -> SA.R n
l2clipVector v clipAmount =
  let norm = norm_2 v
   in if norm > clipAmount
        then SA.dvmap (\elem -> clipAmount * (elem / norm)) v
        else v

instance (ClipGrad x, ClipGrad y) => ClipGrad (Concat m x n y) where
  l2clipGrad clipAmount (Concat c1 c2) = Concat (l2clipGrad clipAmount c1) (l2clipGrad clipAmount c2)

instance ClipGrad (Gradients '[]) where
  l2clipGrad _ GNil = GNil

-- This happens since there's nesting of Gradients.
-- e.g. InceptionMini
instance (ClipGrad (Gradient layer), ClipGrad (Gradients layerTail)) => ClipGrad (Gradients (layer : layerTail)) where
  l2clipGrad clipAmount (grad :/> gradt) = l2clipGrad clipAmount grad :/> l2clipGrad clipAmount gradt

instance ClipGrad () where
  l2clipGrad clipAmount () = ()

instance (ClipGrad a, ClipGrad b) => ClipGrad (a, b) where
  l2clipGrad clipAmount (l, r) = (l2clipGrad clipAmount l, l2clipGrad clipAmount r)

instance (KnownNat i, KnownNat o) => ClipGrad (FullyConnected' i o) where
  l2clipGrad clipAmount (FullyConnected' wB wN) = FullyConnected' (l2clipVector wB clipAmount) (l2clipMatrix wN clipAmount)

-- General purpose combinator
type All :: (k -> Constraint) -> [k] -> Constraint
type family All c xs where
  All _ '[] = ()
  All c (x : xs) = (c x, All c xs)

-- Have a constraint on all gradients
type AllGradients :: (k -> Constraint) -> [k] -> Constraint
type family AllGradients c xs where
  AllGradients _ '[] = ()
  AllGradients c (x : xs) = (c (Gradient x), All c xs)

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

main :: IO ()
main = do
  MnistOpts mnist vali iter rate <- execParser (info (mnist' <**> helper) idm)
  putStrLn "Training convolutional neural network..."

  res <- runExceptT $ convTest iter mnist vali rate
  case res of
    Right () -> pure ()
    Left err -> putStrLn err

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

randomMnist :: MonadRandom m => m MNIST
randomMnist = randomNetwork

convTest :: Int -> FilePath -> FilePath -> LearningParameters -> ExceptT String IO ()
convTest iterations trainFile validateFile rate = do
  net0 <- lift randomMnist
  trainData <- readMNIST trainFile
  validateData <- readMNIST validateFile
  lift $ foldM_ (runIteration trainData validateData) net0 [1 .. iterations]
 where
  -- TODO train calls applyUpdate and backPropagate
  -- Goal: Override those functions
  trainEach rate' !network (i, o) = train rate' network i o

  runIteration trainRows validateRows net i = do
    let trained' = foldl' (trainEach (rate{learningRate = learningRate rate * 0.9 ^ i})) net trainRows
    let res = fmap (\(rowP, rowL) -> (rowL,) $ runNet trained' rowP) validateRows
    let res' = fmap (\(S1D label, S1D prediction) -> (maxIndex (SA.extract label), maxIndex (SA.extract prediction))) res
    print trained'
    putStrLn $ "Iteration " ++ show i ++ ": " ++ show (length (filter ((==) <$> fst <*> snd) res')) ++ " of " ++ show (length res')
    return trained'
