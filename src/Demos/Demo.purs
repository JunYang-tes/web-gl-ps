module Demo where
import Data.Maybe
import Data.Nullable
import Debug
import Demos.DrawElements
import Demos.HelloGL
import Demos.Rotate
import Demos.RotationInsideShader
import Demos.Scale
import Prelude

import Data.Array as Array
import Data.Map (fromFoldable, lookup)
import Data.Tuple.Nested ((/\))
import Demos.DrawArray (mkDrawArrayDemo)
import Demos.Gasket (mkGasket)
import Demos.RotateVector (mkRotateVector)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (ReactComponent)
import React.Basic.DOM as D
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (element)
import React.Basic.Hooks as RH


keys:: Array String
keys = [
    "hello" ,
    "drawArray",
    "cube (drawElements)" ,
    "rotate" ,
    "rotate inside" ,
    "scale",
    "rotate around vector (1,1,1)",
    "Gasket"
]
 

demoComponents = map unsafePerformEffect [
  mkHelloGL,
  mkDrawArrayDemo,
  mkDrawElementDemo,
  mkRotate,
  mkRotateInside,
  mkScaleDemo,
  mkRotateVector,
  mkGasket
]
demos =  fromFoldable $
  Array.zipWith (\n c -> n /\ c) keys demoComponents


mkDemo :: RH.CreateComponent {}
mkDemo = do
  RH.component "Demos" \p -> RH.do
    selected /\ setSelected <- RH.useState (null::(Nullable (ReactComponent(Record()))))
    selectedKey /\ setSelectedKey <- RH.useState ("")
    pure $  D.div {
      className: "demo",
      children:([
        D.ul_ $ map (\k -> 
          D.li { children:  [D.text k] 
                ,className: if selectedKey == k then "selected" else ""
                ,onClick: capture_ do
                  setSelectedKey \_ -> k
                  setSelected \_ -> toNullable $ lookup k demos
          }) keys
      ]<> case toMaybe $ selected of
            Nothing -> [D.div { children: [D.text "No demo selected."],className:"selected-demo"}]
            Just d  -> [D.div { children:[ element d {}], className:"selected-demo" }] )}