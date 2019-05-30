module Demos.DrawElements where
import Prelude
import React.Basic.DOM as D
import React.Basic.Hooks as RH

mkDrawElementDemo :: RH.CreateComponent {}
mkDrawElementDemo = do
  RH.component "CubeMadeByDrawElements" \props -> RH.do
    pure $ D.text "Cube"