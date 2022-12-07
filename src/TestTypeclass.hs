module TestTypeclass where
import Data.Data (Proxy)

-- TODO https://www.haskellforall.com/2021/04/how-to-replace-proxy-with.html

class A a where
  tcmember :: Proxy a -> String

instance A [i] where
  tcmember a = "I am a list"

instance A Integer where
  tcmember a = "I am a integer"

