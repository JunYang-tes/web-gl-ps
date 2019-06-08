module Demos.RotateVector where
import Controls (useSlider)
import Data.Either (Either(..))
import Data.Matrix (i4)
import Data.Matrix.Transform (unsafeRotate)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Vector (flattenV, vec4)
import Debug (debug, debugE)
import Demos.Rotate (ColorCube(..), colorCube)
import Demos.Template (template)
import Effect (Effect)
import Prelude (Unit, bind, discard, pure, unit, ($))
import React.Basic.Hooks as RH
import WebGL (FragmentShader, WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, clear, createBuffer, dm_triangles, drawArrays, enable, enableVertexAttribArray, enable_depth_test, four, getAttribLocation, getUniformLocation, mask_color_buffer_bit, mask_depth_buffer_bit, uniformMat4f, usage_static_draw, vertexAttribPointer, vt_float, (.|.))
vertex :: String
vertex = """
attribute vec4 vPosition;
attribute vec4 vColor; 
varying   vec4 fColor;
uniform   mat4 trans;
void main() {
  fColor = vColor;
  gl_Position = trans * vPosition;
}
"""

fragment:: FragmentShader
fragment = """
precision mediump float;
varying vec4 fColor;
void main() {
  gl_FragColor = fColor;
}
"""

draw :: WebGLRenderingContext -> WebGLProgram -> Number -> Effect Unit
draw gl prog _ = do
  gl `enable` enable_depth_test
  vertexBuffer <- createBuffer gl
  colorBuffer  <- createBuffer gl
  case (vertexBuffer /\ colorBuffer)  of
    (Right buf) /\ (Right colorBuf)-> do
      bindBuffer gl bt_array_buffer buf
      let (ColorCube cs ps) = colorCube
      (bufferData gl bt_array_buffer $ flattenV $ ps) usage_static_draw
      vpos <- getAttribLocation gl prog "vPosition"
      vertexAttribPointer gl vpos four vt_float false 0 0
      enableVertexAttribArray gl vpos
      -- colors
      bindBuffer gl bt_array_buffer colorBuf
      bufferData gl bt_array_buffer (flattenV cs) usage_static_draw
      vcolor <- getAttribLocation gl prog "vColor"
      vertexAttribPointer gl vcolor four vt_float false 0 0
      enableVertexAttribArray gl vcolor
      clear gl (mask_color_buffer_bit .|. mask_depth_buffer_bit)
      -- uniform
      rs <- getUniformLocation gl prog "trans"
      case rs of
        Nothing   -> debugE "Can't get uniform location"
        Just loc  -> do
          uniformMat4f gl loc $ i4 
          drawArrays gl dm_triangles 0 36
    _ -> debugE "Can't create buffer"


update :: WebGLRenderingContext -> WebGLProgram -> Number -> Effect Unit
update gl prog theta = do
  gl `clear` (mask_color_buffer_bit .|. mask_depth_buffer_bit)
  rs <- getUniformLocation gl prog "trans"
  case rs of
    Nothing   -> debugE "Can't get uniform location"
    Just loc  -> do
      uniformMat4f gl loc $ debug "t" $ unsafeRotate (vec4 0.0 0.0 0.0 1.0) (vec4 1.0 1.0 1.0 0.0) theta
      drawArrays gl dm_triangles 0 36

mkRotateVector :: RH.CreateComponent {}
mkRotateVector = template vertex fragment draw update
  RH.do
    theta /\ ele <- useSlider 0.0 0.0 360.0 1.0 "Theta"
    pure $ theta /\ [ele]