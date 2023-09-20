{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import GHC.Generics ((:+:) (L1))
import GHC.TypeLits (KnownNat)
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

-- It's logistic regression!
--
-- This network is used to show how we can embed a Network as a layer in the larger MNIST
-- type.
type FL i o =
  Network
    '[FullyConnected i o, Logit]
    '[ 'D1 i, 'D1 o, 'D1 o]

-- The definition of our convolutional neural network.
-- In the type signature, we have a type level list of shapes which are passed between the layers.
-- One can see that the images we are inputing are two dimensional with 28 * 28 pixels.

-- It's important to keep the type signatures, as there's many layers which can "squeeze" into the gaps
-- between the shapes, so inference can't do it all for us.

-- With the mnist data from Kaggle normalised to doubles between 0 and 1, learning rate of 0.01 and 15 iterations,
-- this network should get down to about a 1.3% error rate.
--
-- /NOTE:/ This model is actually too complex for MNIST, and one should use the type given in the readme instead.
--         This one is just here to demonstrate Inception layers in use.
--

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

-- main :: ExceptT String IO ()
-- main = do
-- initialNetwork <- lift randomMnist
-- trainData <- readMNIST trainFile
-- TODO "run" convTestDP  with net0 trainData

convTestDP ::
  forall e iterations s. -- TODO add this back after layers shapes.
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

-- TODO do we need to override this?
-- backPropagate :: SingI (Last shapes)
--               => Network layers shapes
--               -> S (Head shapes)
--               -> S (Last shapes)
--               -> Gradients layers
-- backPropagate network input target =
--     let (tapes, output) = runNetwork network input
--         (grads, _)      = runGradient network tapes (output - target)
--     in  grads

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
  forall e s. -- TODO this was getting a bit too abstract. add this back later: layers shapes.
  SingI (Last Shapes') =>
  LearningParameters ->
  Network Layers Shapes' ->
  [LabeledInput Shapes'] -> -- Should we be taking in the training data as sensitive list here? or expect the caller to provide it?
  Solo.PM (Solo.TruncatePriv e Solo.Zero s) (Network Layers Shapes')
trainDP rate network trainRows = Solo.do
  -- newtype SList (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SList_UNSAFE {unSList :: [f s]}
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
newtype SGradients (m :: Solo.CMetric) (grads :: [*]) (s :: Solo.SEnv) = SGRAD_UNSAFE {unSGrad :: Gradients grads}

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
  -- This will also need to operate on the hmatrix representation
  -- So just add matrix together
  -- sumListOfGrads gradsl = undefined -- (foldr (+) (matrix . fill 0) listOfGrads) <$> gradsl
  -- Sum a list of Gradients
  sumListOfGrads :: forall layers. All MonoidGradient layers => [Gradients layers] -> Gradients layers
  sumListOfGrads gradsl =
    let headsGrad = heads gradsl
        tailsGrad = tails gradsl
        grad = fold headsGrad
        (next :: Gradients layers) = grad :/> (sumListOfGrads tailsGrad)
     in next
  -- Given a list of the gradient type level list
  -- Return the heads of the type level list
  heads :: [Gradients (layer ': layerTail)] -> [Gradient layer]
  heads ((grad1 :/> _) : t) = grad1 : heads t
  heads [] = []
  -- Ditto but tail
  tails :: [Gradients (layer ': layerTail)] -> [Gradients layerTail]
  tails ((_ :/> gradt) : t) = gradt : tails t

-- Not this: (foldr mappendGrad memptyGrad) <$> gradsl
-- sumListOfGradsHelper :: GradMonoid layer => [Gradients layers] -> Gradients layers
-- sumListOfGradsHelper [grad1 :/> grad1t, grad2 :/> grad2t, grad3 :/> grad3t] =
-- foldr mappendGrad memptyGrad [grad1, gras2, grad3]

-- This could be a Monoid I think but got some compiler errors about using the type family Gradient
-- class GradMonoid x where
--   mappendGrad :: Gradient x -> Gradient x -> Gradient x
--   memptyGrad :: Gradient x -- This would be a matrix/vector for example that would be filled with 0s. Useful for the fold

-- class GradMonoid x where
--   mappendGrad :: x -> x -> x
--   memptyGrad :: x -- This would be a matrix/vector for example that would be filled with 0s. Useful for the fold

-- instance (KnownNat i, KnownNat o) => Monoid (FullyConnected' i o) where
--   (FullyConnected' wB wN) <> (FullyConnected' wB2 wN2) = FullyConnected' (wB + wB2) (wN + wN2)

-- Credit to Morrow on FP Discord
-- General purpose combinator
type All :: (k -> Constraint) -> [k] -> Constraint
type family All c xs where
  All _ '[] = ()
  All c (x : xs) = (c x, All c xs)

-- Constraint synonym
class Monoid (Gradient layer) => MonoidGradient layer
instance Monoid (Gradient layer) => MonoidGradient layer

class UpdateLayer (Gradient layer) => UpdateLayerGradient layer
instance UpdateLayer (Gradient layer) => UpdateLayerGradient layer

-- TODO make SemiGroup
instance (KnownNat i, KnownNat o, Semigroup (FullyConnected' i o)) => Monoid (FullyConnected' i o) where
  mappend (FullyConnected' wB wN) (FullyConnected' wB2 wN2) = FullyConnected' (wB + wB2) (wN + wN2)

  mempty = FullyConnected' (SA.vector [0 .. 0]) (SA.matrix [0 .. 0])

-- This is an example of a layer where the Gradient is ()
-- instance Monoid () where
--   mappend () () = ()
--   mempty = ()

-- TODO fill in the rest of the instances

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
