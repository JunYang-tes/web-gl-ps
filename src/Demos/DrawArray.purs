module Demos.DrawArray where
import Demos.Template

import Controls (useRadioGroup)
import Data.Array.NonEmpty (NonEmptyArray, findIndex, unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (debugE)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, eq, negate, pure, ($))
import React.Basic.Hooks as RH
import Unsafe.Coerce (unsafeCoerce)
import WebGL (DrawMode, FragmentShader, VertexShader, WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, createBuffer, dm_line_loop, dm_line_strip, dm_lines, dm_triangle_fan, dm_triangle_strip, dm_triangles, drawArrays, enableVertexAttribArray, getAttribLocation, two, usage_static_draw, vertexAttribPointer, vt_float)

vertex :: VertexShader
vertex = """
attribute vec4 vPosition;
void main() {
  gl_Position = vPosition;
}
"""
fragment :: FragmentShader
fragment = """
precision mediump float;
void main() {
  gl_FragColor = vec4(1.0,0.0,0.0,1.0);
}
"""

draw :: WebGLRenderingContext -> WebGLProgram ->
  DrawMode -> Effect Unit
draw gl prog dm = do
  buffer <- createBuffer gl
  case buffer of
    (Left err) -> debugE err
    (Right buf) -> do
      bindBuffer gl bt_array_buffer buf
      bufferData gl bt_array_buffer [(-0.3),(-0.3),
        (-0.3),0.3,
        (-0.2),(-0.2),
        (-0.2),0.2,
        (-0.1),(-0.1),
        (-0.1),0.1,
        0.0,0.0,
        0.1,(-0.1),
        0.1,(0.1),
        0.2,(-0.2),
        0.2,(0.2),
        0.3,(-0.3),
        0.3,(0.3)
        ] usage_static_draw
      pos <- getAttribLocation gl prog "vPosition"
      vertexAttribPointer gl pos two vt_float false 0 0
      enableVertexAttribArray gl pos
      debugE "draw array"
      drawArrays gl dm 0 13
    
update :: WebGLRenderingContext -> WebGLProgram ->  DrawMode -> Effect Unit
update gl _ dm = do
  drawArrays gl dm 0 13

drawModeNames :: NonEmptyArray String
drawModeNames = unsafeCoerce $ ["lines","line loop","line strip","triangle fan",
    "triangle strip","triangles"
  ]

drawModes:: NonEmptyArray DrawMode
drawModes = unsafeCoerce $ [dm_lines,dm_line_loop,dm_line_strip,dm_triangle_fan
  ,dm_triangle_strip
  ,dm_triangles
]

mkDrawArrayDemo :: RH.CreateComponent {}
mkDrawArrayDemo = template vertex fragment draw update 
  RH.do
    name /\ ele <-useRadioGroup drawModeNames
    pure $ case findIndex (eq name) drawModeNames of
      Nothing -> (unsafePartial $ unsafeIndex drawModes 0) /\ [ele]
      Just i  -> (unsafePartial $ unsafeIndex drawModes i) /\ [ele]
 