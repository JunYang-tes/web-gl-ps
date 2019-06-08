module Demos.DrawElements where
-- import Data.Array
import Controls
import Data.Either
import Data.Maybe
import Data.Nullable
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Prelude
import WebGL
import WebGL.React

import Data.Matrix.Transform (rotateXM4, rotateYM4, rotateZM4)
import Data.Vector (flattenV)
import Debug (debugE)
import Demos.Rotate (colorList, vertices)
import Effect (Effect)
import React.Basic.DOM as D
import React.Basic.Hooks as RH
import Demos.Template

vertex :: String
vertex = """
attribute vec4 vPosition;
attribute vec4 vColor;
uniform   mat4 transform;
varying   vec4 fColor;

void main() {
  fColor = vColor;
  gl_Position = transform * vPosition;
}
"""

fragment:: String
fragment = """
precision mediump float;
varying vec4 fColor;
void main () {
  gl_FragColor = fColor;
}
"""

draw:: WebGLRenderingContext -> WebGLProgram -> (Number /\ Number /\ Number)-> Effect Unit
draw gl prog _ = do
  gl `enable` enable_depth_test
  vertxBuffer <- createBuffer gl
  colorBuffer <- createBuffer gl
  indBuffer   <- createBuffer gl
  case (vertxBuffer /\ colorBuffer /\ indBuffer) of
    (Right vbuf) /\ (Right cbuffer) /\ (Right ibuffer) -> do
      bindBuffer gl bt_array_buffer vbuf
      bufferData gl bt_array_buffer 
        (flattenV vertices) usage_static_draw
      
      vpos <- getAttribLocation gl prog "vPosition"
      vertexAttribPointer gl vpos four vt_float false 0 0
      enableVertexAttribArray gl vpos
      
      bindBuffer gl bt_array_buffer cbuffer
      bufferData gl bt_array_buffer
        (flattenV colorList) usage_static_draw
      vcolor <- getAttribLocation gl prog "vColor"
      vertexAttribPointer gl vcolor four vt_float false 0 0
      enableVertexAttribArray gl vcolor
      
      bindBuffer gl bt_element_array_buffer ibuffer
      let ind = [1,0,3,
         3,2,1,
         2,3,7,
         7,6,2,
         3,0,4,
         4,7,3,
         6,5,1,
         1,2,6,
         4,5,6,
         6,7,4,
         5,4,0,
         0,1,5
        ]
      bufferDataUInt8 gl bt_element_array_buffer ind
        
        usage_static_draw
      bindBuffer gl bt_element_array_buffer ibuffer
      rs <- getUniformLocation gl prog "transform"
      case rs of
        Nothing -> debugE "Can't get unfiorm location"
        Just loc -> do
          uniformMat4f gl loc $ ((rotateXM4 0.0)*(rotateYM4 0.0) * (rotateZM4 0.0))
          -- 36 = length of ind * 3\
          drawElements gl dm_triangles 36 unsigned_byte 0
          debugE "drawElements called"
    _ -> debugE "Can't create buffer"
  
update :: WebGLRenderingContext -> WebGLProgram 
  ->(Number /\ Number /\ Number) -> Effect Unit
update gl prog (rx /\ ry /\ rz) = do
  gl `clear` (mask_color_buffer_bit .|. mask_depth_buffer_bit)
  rs <- getUniformLocation gl prog "transform"
  case rs of
    Nothing -> debugE "Can't get unfiorm location"
    Just loc -> do
      uniformMat4f gl loc $ ((rotateXM4 rx)*(rotateYM4 ry) * (rotateZM4 rz))
      drawElements gl dm_triangles 36 unsigned_byte 0

mkDrawElementDemo :: RH.CreateComponent {}
mkDrawElementDemo = template vertex fragment draw update
  RH.do
    rx /\ rx_ele <- useSlider 0.0 0.0 360.0 1.0 "rotate X"
    ry /\ ry_ele <- useSlider 0.0 0.0 360.0 1.0 "rotate Y"
    rz /\ rz_ele <- useSlider 0.0 0.0 360.0 1.0 "rotate Z"
    pure $ (rx /\ ry /\ rz) /\ [rx_ele,ry_ele,rz_ele]
