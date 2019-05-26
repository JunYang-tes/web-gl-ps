module Controls where
import Prelude
import React.Basic (JSX)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue,capture)
import Data.Tuple(Tuple(..))
import Data.Tuple.Nested((/\))
import Data.Maybe
import Effect.Unsafe (unsafePerformEffect)
import Data.Number (fromString)
import Data.Functor.Indexed
import React.Basic.Hooks as RH
import Debug

useSlider:: forall hooks. Number -> Number -> Number -> Number -> String 
  ->RH.Render hooks (RH.UseState Number hooks) (Tuple Number JSX)

useSlider init min max step label = RH.do
  state /\ setState <- RH.useState init
  pure $ (state /\ D.div_ [
    D.label_ [D.text label],
    D.input { min,
              max,
              onChange: capture targetValue (\t ->pure $
                case (join $ t <#> fromString) of
                  Nothing -> unit
                  Just val-> unsafePerformEffect $ setState \_ -> val
                ),
              type:"range",
              step: show step,
              value: show state
            },
    D.text $ show state
  ])