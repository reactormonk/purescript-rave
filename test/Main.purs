module Test.Main where

import Prelude
import Rave

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Type.Row (type (+))

-- Into Rave
type ErrorData a r = (error :: a | r)

-- Into a supreme library

logFun :: String -> Rave ErrConfig () Unit
logFun str = do
  c <- ask
  liftEffect $ c.logFun str

type ErrConfig = { logFun :: forall c. String -> Effect Unit }
type HandleLog r = (handleLog :: Rave ErrConfig () Unit | r)

-- Into the library implementing cache, ES, etc.

type Config r = r
type Errors e = ( err1 :: Record (ErrorData Int + HandleLog + ()) | e)

main :: Effect Unit
main = do
  log "You should add some tests."

failing1 :: forall r e. Rave (Config r) (Errors e) Int
failing1 = do
  -- try something
  let errData = 5
  throw
    ({ err1 :
      ({ error: errData
      , handleLog: logFun $ "Got error: " <> (show errData)
      } :: Record (ErrorData Int + HandleLog + ()))
    } :: Record (Errors e))
