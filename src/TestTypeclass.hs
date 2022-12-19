module TestTypeclass where
import Data.Data (Proxy)

-- TODO https://www.haskellforall.com/2021/04/how-to-replace-proxy-with.html

class A a where
  tcmember :: Proxy a -> String

instance A [i] where
  tcmember a = "I am a list"

instance A Integer where
  tcmember a = "I am a integer"

-- TODO try Joe's thing without a proxy and see if it works
-- Question how does this approach know what a is
-- Well I think it actually might when you invoke the function
-- I'm guessing the error might not be too useful

class Distance a where
  distance :: a -> a -> Double

instance Distance [i] where
  distance a b = 3

instance Distance Integer where
  distance a b = 3