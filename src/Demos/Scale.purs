module Demos.Scale where
import Controls (useSlider)
import Data.Either (Either(..))
import Data.Matrix.Transform (rotateXM4, rotateYM4, rotateZM4, scaleX, scaleY, scaleZ)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toNullable)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vector (flattenV)
import Debug (debugE)
import Demos.Rotate (ColorCube(..), colorCube)
import Effect (Effect)
import Prelude
import React.Basic.DOM as D
import React.Basic.Hooks as RH
import WebGL (FragmentShader, VertexShader, WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, clear, createBuffer, dm_triangles, drawArrays, enable, enableVertexAttribArray, enable_depth_test, four, getAttribLocation, getUniformLocation, mask_color_buffer_bit, mask_depth_buffer_bit, uniform3f, uniformMat4f, usage_static_draw, vertexAttribPointer, vt_float, (.|.))
import WebGL.React (ProgramReady, mkWebGLWithShaders)

vertex :: VertexShader
vertex = """
attribute vec4 vPosition;
attribute vec4 vColor; 
varying   vec4 fColor;
uniform   mat4 scale;
void main() {
  fColor = vColor;
  gl_Position = scale * vPosition;
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
draw:: WebGLRenderingContext -> WebGLProgram -> Effect Unit
draw gl prog = do
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
      rs <- getUniformLocation gl prog "scale"
      case rs of
        Nothing   -> debugE "Can't get uniform location"
        Just loc  -> do
          uniformMat4f gl loc $ scaleX 1.0
          drawArrays gl dm_triangles 0 36
          pure unit
          drawArrays gl dm_triangles 0 36
    _ -> debugE "Can't create buffer"

update :: WebGLRenderingContext -> WebGLProgram -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
update gl prog sx sy sz rx ry rz = do
  gl `clear` (mask_color_buffer_bit .|. mask_depth_buffer_bit)
  rs <- getUniformLocation gl prog "scale"
  case rs of
    Nothing   -> debugE "Can't get uniform location"
    Just loc  -> do
      uniformMat4f gl loc $ ((rotateXM4 rx) * (rotateYM4 ry) * (rotateZM4 rz) * (scaleX sx) * (scaleY sy) * (scaleZ sz))
      drawArrays gl dm_triangles 0 36
      pure unit


mkScaleDemo :: RH.CreateComponent {}
mkScaleDemo = do
  webgl <- mkWebGLWithShaders:: Effect (
    RH.ReactComponent (Record
      (vertex::VertexShader
      ,fragment:: FragmentShader
      ,onReady:: ProgramReady
      )
    )
  )
  RH.component "Rotate" \p -> RH.do
    rx /\ rx_ele <- useSlider 0.0 0.0 360.0 1.0 "rotate X"
    ry /\ ry_ele <- useSlider 0.0 0.0 360.0 1.0 "rotate Y"
    rz /\ rz_ele <- useSlider 0.0 0.0 360.0 1.0 "roate Z"
    sx /\ sx_ele <- useSlider 1.0 0.0 1.5 0.01 "scale x"
    sy /\ sy_ele <- useSlider 1.0 0.0 1.5 0.01 "Y"
    sz /\ sz_ele <- useSlider 1.0 (-1.0) 1.5 0.01 "Z"
    glRef <- RH.useRef (null::(Nullable (Tuple WebGLRenderingContext WebGLProgram)))
    glRef_ <-RH.renderRefMaybe glRef
    RH.useEffect [sx,sy,sz,rx,ry,rz] do
      case glRef_ of
        Nothing -> pure unit
        Just (gl /\ prog) -> do
          update gl prog sx sy sz rx ry rz
      pure $ pure $unit

    pure $ D.div_ [sx_ele
      ,sy_ele
      ,sz_ele
      ,rx_ele
      ,ry_ele
      ,rz_ele
      ,RH.element webgl {vertex
        ,onReady: (\gl prog->do
          RH.writeRef glRef $ toNullable $ Just $ gl /\ prog
          draw gl prog
          pure $ unit
        )
        ,fragment}
    ]

