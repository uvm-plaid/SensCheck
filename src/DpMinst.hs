{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DpMinst where

-- Code from example of MNIST: https://github.com/HuwCampbell/grenade/blob/master/examples/main/mnist.hs
-- However with DP TODO

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Except
import Data.Attoparsec.Text qualified as A
import Data.List (foldl')
#if ! MIN_VERSION_base(4,13,0)
import           Data.Semigroup ( (<>) )
#endif
import Data.Singletons.Decide
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Storable qualified as V
import GHC.Generics ((:+:) (L1))
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
  '[ Reshape,
     Concat ('D3 28 28 1) Trivial ('D3 28 28 14) (InceptionMini 28 28 1 5 9),
     Pooling 2 2 2 2,
     Relu,
     Concat ('D3 14 14 3) (Convolution 15 3 1 1 1 1) ('D3 14 14 15) (InceptionMini 14 14 15 5 10),
     Crop 1 1 1 1,
     Pooling 3 3 3 3,
     Relu,
     Reshape,
     FL 288 80,
     FL 80 10
   ]

-- The shape
type Shapes' =
  '[ 'D2 28 28,
     'D3 28 28 1,
     'D3 28 28 15,
     'D3 14 14 15,
     'D3 14 14 15,
     'D3 14 14 18,
     'D3 12 12 18,
     'D3 4 4 18,
     'D3 4 4 18,
     'D1 288,
     'D1 80,
     'D1 10
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
      let trained' = foldl' (trainEach (rate {learningRate = learningRate rate * 0.9 ^ i})) net trainRows
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
  forall e iterations s. -- TODO add this back ater layers shapes.
  (TL.KnownNat iterations) =>
  [LabeledInput Shapes'] ->
  Network Layers Shapes' ->
  LearningParameters ->
  Solo.PM (Solo.ScalePriv (Solo.TruncatePriv e Solo.Zero s) iterations) (Network Layers Shapes') -- ExceptT String IO ()
convTestDP trainData initialNetwork rate = do
  Solo.seqloop @iterations (runIteration trainData) initialNetwork
  where
    runIteration trainRows i net = do
      let trained' = trainDP @e @s (rate {learningRate = learningRate rate * 0.9 ^ i}) net trainRows
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

-- TODO override this to do DP things
-- Update a network with new weights after training with an instance.
trainDP ::
  forall ε s. -- TODO this was getting a bit too abstract. add this back later: layers shapes.
  SingI (Last Shapes') =>
  LearningParameters ->
  Network Layers Shapes' ->
  [LabeledInput Shapes'] -> -- Should we be taking in the training data as sensitive list here? or expect the caller to provide it?
  Solo.PM (Solo.TruncatePriv ε Solo.Zero s) (Network Layers Shapes')
trainDP rate network trainRows = do
  -- newtype SList (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SList_UNSAFE {unSList :: [f s]}
  let sensitiveTrainRows = Solo.SList_UNSAFE @Solo.L2 @_ @s $ STRAINROW_UNSAFE @Solo.Disc @Shapes' <$> trainRows
      gradSum = clippedGrad network sensitiveTrainRows
      gradSumUnsafe = Solo.D_UNSAFE $ gradsToDouble $ unSGrad gradSum -- Turn this to a number?
  noisyGrad <- Solo.laplace @ε @s gradSumUnsafe -- Who knows what will go here????
  return $ applyUpdate rate network (noisyGrad / (length trainRows)) -- TODO this expects Gradients not a single gradient
  where
    -- I am confused here.
    -- Gradients is a list of Gradient
    -- I do a destrucutre to get a single Gradient x
    -- But Gradient is polymorphic on some x?
    -- So how would I make it a Double?
    gradsToDouble :: Gradients Layers -> Double
    gradsToDouble (gradient :/> gradientRest) = undefined

-- TODO reference this:
-- https://hackage.haskell.org/package/grenade-0.1.0/docs/src/Grenade-Core-Network.html#Network
newtype SGradients (m :: Solo.CMetric) (grads :: [*]) (s :: Solo.SEnv) = SGRAD_UNSAFE {unSGrad :: Gradients grads}

-- The training row
newtype STrainRow (m :: Solo.NMetric) shapes (s :: Solo.SEnv) = STRAINROW_UNSAFE {unSTrainRow :: LabeledInput shapes}

-- THIS IS THE PIECE TO RUN SENSCHECK ON
--                                                     needs to be a sensitive type
--                                                    vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
clippedGrad ::
  Network layers shapes ->
  Solo.SList Solo.L2 (STrainRow Solo.Disc shapes) senv ->
  SGradients Solo.L2 layers senv -- need SGradients or simpler rep of gradients
clippedGrad network trainRows =
  -- For every training example, backpropagate and clip the gradients
  let grads = oneGrad . unSTrainRow <$> Solo.unSList trainRows -- I think we want to map here not fold?
      clippedGrads = map l2clip grads
      gradSum = columnSum clippedGrads -- sum the gradients, column-wise, to get 1-sensitive val
   in SGRAD_UNSAFE gradSum
  where
    oneGrad (i, o) = backPropagate network i o
    l2clip = undefined -- TODO
    columnSum = undefined -- TODO

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
