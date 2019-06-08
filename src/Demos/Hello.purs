module Demos.HelloGL where
import Prelude (Unit, bind, discard, negate, pure, unit, ($))
import React.Basic.Hooks as RH
import Data.Tuple.Nested (type (/\), (/\))
import Controls (useSlider)
import WebGL (WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, clearColor, createBuffer, dm_line_strip, drawArrays, enableVertexAttribArray, getAttribLocation, two, usage_static_draw, vertexAttribPointer, vt_float)
import Effect (Effect)
import Data.Either (Either(..))
import Debug (debugE)

import Demos.Template (template)

vertex :: String
vertex = """ 
attribute vec4 vPosition;
void main() {
  gl_Position = vPosition;
}
"""

fragment :: String
fragment = """
precision mediump float;
void main() {
  gl_FragColor = vec4(1.0,0.0,0.0,1.0);
}
"""

helloWebGL:: WebGLRenderingContext -> WebGLProgram ->
  (Number /\ Number) ->
 Effect Unit
helloWebGL gl prog _ = do
  clearColor 0.0 0.0 0.0 0.0 gl
  buffer <- createBuffer gl
  case buffer of
    (Left err) -> debugE err
    (Right buf) -> do
      debugE "Buffer created"
      bindBuffer gl bt_array_buffer buf
      bufferData gl bt_array_buffer [0.0,0.0,1.0,1.0] usage_static_draw
      pos <- getAttribLocation gl prog "vPosition"
      vertexAttribPointer gl pos two vt_float false 0 0
      enableVertexAttribArray gl pos
      drawArrays gl dm_line_strip 0 2
      pure unit

updateGL:: WebGLRenderingContext -> WebGLProgram -> (Number /\ Number )-> Effect Unit
updateGL gl prog (y /\ x) = do
  buffer <- createBuffer gl
  case buffer of
    (Left err) -> debugE err
    (Right buf) -> do
      bindBuffer gl bt_array_buffer buf
      bufferData gl bt_array_buffer [0.0,0.0,x,y] usage_static_draw
      pos <- getAttribLocation gl prog "vPosition"
      vertexAttribPointer gl pos two vt_float false 0 0
      enableVertexAttribArray gl pos
      drawArrays gl dm_line_strip 0 2
      pure unit

mkHelloGL :: RH.CreateComponent {}
mkHelloGL = template vertex fragment helloWebGL updateGL (RH.do
  x /\ xele <- useSlider 1.0 (-1.0) 1.0 0.01 "X"
  y /\ yele <- useSlider 1.0 (-1.0) 1.0 0.01 "Y"
  pure $ ((x /\ y) /\ [xele,yele])
)

    