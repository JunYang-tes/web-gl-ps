module Controls where
import Data.Array.NonEmpty
import Data.Functor.Indexed
import Data.Maybe
import Debug
import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Number (fromString)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX)
import React.Basic.DOM as D
import React.Basic.DOM.Events (capture, capture_, targetValue)
import React.Basic.Hooks as RH

useRadioGroup:: forall hooks. NonEmptyArray String  ->
  RH.Render hooks (RH.UseState String hooks) (Tuple String JSX)
useRadioGroup ns = RH.do  
  state /\ setState <- RH.useState $ head ns
  pure $ (state /\ (D.div_ $ toArray $
    map (\txt -> D.div_ [
      D.label_ [
        D.div {
            className:"checkbox " <> if txt==state then "checked" else "unchecked"
            ,onClick: capture_ do
              setState (\_ -> txt)
          }
        ,D.text txt
      ]
      ]) ns ))

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