{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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

-- Code from example of MNIST: https://github.com/HuwCampbell/grenade/blob/master/examples/main/mnist.hs
-- However with DP TODO

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Except
import Data.Attoparsec.Text qualified as A
import Data.Foldable (fold)
import Data.Kind (Constraint)
import Data.List (foldl')
import Data.List.Singletons (Tail)
import Data.Singletons.Decide
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Storable qualified as V
import GHC.Base (Type)
import GHC.Generics ((:+:) (L1))
import GHC.TypeLits
import GHC.TypeLits qualified as TL
import Grenade
import Grenade.Utils.OneHot
import Numeric.LinearAlgebra (maxIndex)
import Numeric.LinearAlgebra.Static qualified as SA
import Options.Applicative
import Prelude.Singletons (Head, Last, SingI)
import Primitives qualified as Solo
import Privacy qualified as Solo
import Sensitivity qualified as Solo
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

-- Gradients refers to one actual gradient for a single training example
-- [Gradients] or whatever the actual type is the whole Gradient
-- Gradient is actually not a gradient

-- TODO ideally all the unsafe stuff is in clipped grad
-- All the safe stuff will be in this function
-- TODO override this to do DP things
-- Update a network with new weights after training with an instance.
trainDP ::
  forall e s layers shapes.
  SingI (Last Shapes') =>
  LearningParameters ->
  Network Layers Shapes' ->
  [LabeledInput Shapes'] -> -- Should we be taking in the training data as sensitive list here? or expect the caller to provide it?
  Solo.PM (Solo.TruncatePriv e Solo.Zero s) (Network Layers Shapes')
trainDP rate network trainRows = Solo.do
  -- newtype SList (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SList_UNSAFE {unSList :: [f s]}
  -- TODO move this inside clipped grad
  let sensitiveTrainRows = Solo.SList_UNSAFE @Solo.L2 @_ @s $ STRAINROW_UNSAFE @Solo.Disc @Shapes' <$> trainRows
      gradSum = clippedGrad network sensitiveTrainRows
  -- gradSumUnsafe = Solo.D_UNSAFE $ gradsToDouble $ unSGrad gradSum -- Turn this to a number?
  noisyGrad <- laplaceGradients @e @s gradSum -- Who knows what will go here????
  -- return $ applyUpdate rate network (noisyGrad / (length trainRows)) -- TODO this expects Gradients not a single gradient
  Solo.return $ applyUpdate rate network noisyGrad -- TODO divide by length trainRows
  --  where
  -- I am confused here.
  -- Gradients is a list of Gradient
  -- I do a destrucutre to get a single Gradient x
  -- But Gradient is polymorphic on some x?
  -- So how would I make it a Double?
  -- getGradientMatrix :: Gradients Layers -> _
  -- getGradientMatrix (gradient :/> gradientRest) = undefined

-- TODO reference this:
-- https://hackage.haskell.org/package/grenade-0.1.0/docs/src/Grenade-Core-Network.html#Network
newtype SGradients (m :: Solo.CMetric) (grads :: [Type]) (s :: Solo.SEnv) = SGRAD_UNSAFE {unSGrad :: Gradients grads}

-- SHould return a non-sensitive Gradient
laplaceGradients :: forall e s. SGradients Solo.L2 Layers s -> Solo.PM (Solo.TruncatePriv e Solo.Zero s) (Gradients Layers)
laplaceGradients gradient = undefined

-- The training row
newtype STrainRow (m :: Solo.NMetric) shapes (s :: Solo.SEnv) = STRAINROW_UNSAFE {unSTrainRow :: LabeledInput shapes}

-- THIS IS THE PIECE TO RUN SENSCHECK ON
-- We are going to take a list of gradients and turn it into a single gradient
-- Takes all the training examples and computes gradients
-- Clips it
--
clippedGrad ::
  forall senv.
  Network Layers Shapes' ->
  Solo.SList Solo.L2 (STrainRow Solo.Disc Shapes') senv ->
  SGradients Solo.L2 Layers senv -- need SGradients or simpler rep of gradients
clippedGrad network trainRows =
  -- For every training example, backpropagate and clip the gradients
  let grads = oneGrad . unSTrainRow <$> Solo.unSList trainRows
      clippedGrads = l2clip <$> grads
      -- sum the gradients, column-wise, to get 1-sensitive val
      gradSum = sumListOfGrads clippedGrads
   in SGRAD_UNSAFE @Solo.L2 @_ @senv gradSum
 where
  -- Takes single training example and calculates the gradient for that training example
  oneGrad (example, label) = backPropagate network example label
  -- TODO at some point I will actually need to operate on the hmatrix representation
  l2clip :: Gradients Layers -> Gradients Layers
  l2clip = undefined -- TODO
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

class SumListOfGrads layers where
  sumListOfGrads :: [Gradients layers] -> Gradients layers

instance SumListOfGrads '[] where
  sumListOfGrads _ = GNil

instance (Monoid (Gradient layer), UpdateLayer layer, SumListOfGrads layerTail) => SumListOfGrads (layer : layerTail) where
  sumListOfGrads gradsl =
    let headsGrad = heads gradsl
        tailsGrad = tails gradsl
        (grad :: Gradient layer) = fold headsGrad
        (next :: Gradients (layer ': layerTail)) = grad :/> sumListOfGrads tailsGrad
     in next

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

-- This happens since there's nesting of Gradients.
-- e.g. InceptionMini
instance (SumListOfGrads (layer : layerTail)) => Semigroup (Gradients (layer : layerTail)) where
  grad1 <> grad2 = sumListOfGrads [grad1, grad2]

instance (SumListOfGrads (layer : layerTail)) => Monoid (Gradients (layer : layerTail)) where
  -- TODO not sure about this one
  mempty = sumListOfGrads []

instance (KnownNat i, KnownNat o) => Semigroup (FullyConnected' i o) where
  (FullyConnected' wB wN) <> (FullyConnected' wB2 wN2) = FullyConnected' (wB + wB2) (wN + wN2)

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
  (<>) ::
    ( KnownNat channels
    , KnownNat filters
    , KnownNat kernelRows
    , KnownNat kernelColumns
    , KnownNat strideRows
    , KnownNat strideColumns
    ) =>
    Convolution'
      channels
      filters
      kernelRows
      kernelColumns
      strideRows
      strideColumns ->
    Convolution'
      channels
      filters
      kernelRows
      kernelColumns
      strideRows
      strideColumns ->
    Convolution'
      channels
      filters
      kernelRows
      kernelColumns
      strideRows
      strideColumns
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

instance (Semigroup x, Semigroup y) => Semigroup (Concat m x n y) where
  (Concat x1 y1) <> (Concat x2 y2) = Concat (x1 <> x2) (y1 <> y2)

instance (Monoid x, Monoid y) => Monoid (Concat m x n y) where
  mempty = Concat mempty mempty

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
