module Debug(
  debug
  ,debugE
  ,debugEWithTag
  ,debugc
) where
import Prelude

import Effect (Effect)

foreign import debug
  :: forall a. String -> a -> a

debugc :: forall a. Boolean -> String -> a -> a
debugc p tag v = if p then (debug tag v) else v

foreign import debugE :: forall a. a -> Effect (Unit)

foreign import debugEWithTag ::forall a. String -> a -> Effect  (Unit)
