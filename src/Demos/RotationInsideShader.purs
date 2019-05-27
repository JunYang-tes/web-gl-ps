module Demos.RotationInsideShader where
import Controls (useSlider)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toNullable)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vector (flattenV)
import Debug (debugE)
import Demos.Rotate (ColorCube(..), colorCube)
import Effect (Effect)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>))
import React.Basic.DOM as D
import React.Basic.Hooks as RH
import WebGL (FragmentShader, VertexShader, WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, clear, clear_color_buffer_bit, clear_depth_buffer_bit, createBuffer, dm_triangles, drawArrays, enable, enableVertexAttribArray, enable_depth_test, four, getAttribLocation, getUniformLocation, mask_color_buffer_bit, mask_depth_buffer_bit, uniform3f, usage_static_draw, vertexAttribPointer, vt_float, (.|.))
import WebGL.React (ProgramReady, mkWebGLWithShaders)

vertex :: VertexShader
vertex = """
attribute vec4 vPosition;
attribute vec4 vColor; 
varying   vec4 fColor;
uniform   vec3 theta;
void main() {
  fColor = vColor;
  vec3 angles = radians (theta);
  vec3 c = cos(angles);
  vec3 s = sin(angles);
  // matrices are column-major
  mat4 rx = mat4(
    1.0, 0.0, 0.0, 0.0,
    0.0, c.x, s.x, 0.0,
    0.0,-s.x, c.x, 0.0,
    0.0, 0.0, 0.0, 1.0
  );
  mat4 ry = mat4(
    c.y, 0.0, -s.y, 0.0,
    0.0, 1.0,  0.0, 0.0,
    s.y, 0.0,  c.y, 0.0,
    0.0, 0.0,  0.0, 1.0
  );
  mat4 rz = mat4(
    c.z, -s.z, 0.0, 0.0,
    s.z, c.z,  0.0, 0.0,
    0.0, 0.0,  1.0, 0.0,
    0.0, 0.0,  0.0, 1.0
  );
  gl_Position = rx * ry * rz * vPosition;
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
      rs <- getUniformLocation gl prog "theta"
      case rs of
        Nothing   -> debugE "Can't get uniform location"
        Just loc  -> do
          uniform3f gl loc 0.0 0.0 0.0
          drawArrays gl dm_triangles 0 36
          pure unit
    _ -> debugE "Can't create buffer"

update :: WebGLRenderingContext -> WebGLProgram -> Number -> Number -> Number -> Effect Unit
update gl prog rx ry rz = do
  gl `clear` (mask_color_buffer_bit .|. mask_depth_buffer_bit)
  rs <- getUniformLocation gl prog "theta"
  case rs of
    Nothing   -> debugE "Can't get uniform location"
    Just loc  -> do
      uniform3f gl loc rx ry rz
      drawArrays gl dm_triangles 0 36
      pure unit


mkRotateInside :: RH.CreateComponent {}
mkRotateInside = do
  webgl <- mkWebGLWithShaders:: Effect (
    RH.ReactComponent (Record
      (vertex::VertexShader
      ,fragment:: FragmentShader
      ,onReady:: ProgramReady
      )
    )
  )
  RH.component "Rotate" \p -> RH.do
    rx /\ rx_ele <- useSlider 0.0 0.0 360.0 1.0 "X"
    ry /\ ry_ele <- useSlider 0.0 0.0 360.0 1.0 "Y"
    rz /\ rz_ele <- useSlider 0.0 0.0 360.0 1.0 "Z"
    glRef <- RH.useRef (null::(Nullable (Tuple WebGLRenderingContext WebGLProgram)))
    glRef_ <-RH.renderRefMaybe glRef
    RH.useEffect (show rx <> show ry <> show rz) do
      pure case glRef_ of
        Nothing -> pure $ unit
        Just (gl /\ prog) -> do
          debugE ("update" <> (show rx))
          update gl prog rx ry rz
          pure $ unit

    pure $ D.div_ [rx_ele
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

