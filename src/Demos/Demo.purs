module Demo where
import Data.Maybe
import Data.Tuple.Nested((/\))
import Data.Map (fromFoldable,lookup)
import Data.Array as Array
import Data.Nullable
import Demos.HelloGL
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks(element)
import React.Basic.DOM.Events(capture_)
import React.Basic (ReactComponent)
import React.Basic.Hooks as RH
import React.Basic.DOM as D
import Debug
import Prelude
import Demos.Rotate
import Demos.RotationInsideShader
import Demos.Scale
import Demos.DrawElements


keys:: Array String
keys = [
    "hello" ,
    "cube (drawElements)" ,
    "rotate" ,
    "rotate inside" ,
    "scale"
]
 

demoComponents = map unsafePerformEffect [
  mkHelloGL,
  mkDrawElementDemo,
  mkRotate,
  mkRotateInside,
  mkScaleDemo
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