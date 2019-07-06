module Demos.LookAt where
import Data.Tuple.Nested
import Demos.Rotate
import Demos.Template
import Demos.Template
import Prelude
import WebGL

import Controls (useVector3)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Gasket3D (divided3)
import Data.Matrix (class MatrixOps, M4(..), Matrix, flattenM, i4)
import Data.Matrix.Transform (lookAt)
import Data.Maybe (Maybe(..))
import Data.Vector (flattenV, vec3)
import Debug (debug, debugE)
import Effect (Effect)
import React.Basic.DOM (css, div, style)
import React.Basic.Hooks as RH


vertex :: String
vertex = """
attribute  vec4 vPosition;
attribute  vec4 vColor;
varying vec4 fColor;

uniform mat4 lookAt;

void main() 
{
    fColor = vColor;
    gl_Position = lookAt * vPosition;
} 
"""

fragment :: String
fragment = """ 
precision mediump float;
varying vec4 fColor;
void main () {
  gl_FragColor = fColor;
}
"""

draw :: WebGLRenderingContext -> WebGLProgram -> Matrix M4 -> Effect Unit
draw gl prog m = do
  gl `enable` enable_depth_test
  let (ColorCube cs ps) = colorCube
  psBuffer <- createBuffer gl
  csBuffer <- createBuffer gl
  case (psBuffer /\ csBuffer) of
    (Right pb) /\ (Right cb) -> do
      bufData pb  "vPosition" (flattenV ps) four
      bufData cb "vColor" (flattenV cs) four

      lookAtPos <- getUniformLocation gl prog "lookAt"
      case lookAtPos of
        Nothing -> debugE "Can't get uniform location"
        Just loc -> do
          uniformMat4f gl loc m
          drawArrays gl dm_triangles 0 $ length ps

    otherwise -> debugE "Couldn't create buffer"
  where
    bufData :: WebGLBuffer -> String -> Array Number -> 
      Dimension ->
      Effect Unit
    bufData buf name d dim = do
      bindBuffer gl bt_array_buffer buf
      bufferData gl bt_array_buffer d usage_static_draw
      pos <- getAttribLocation gl prog name
      vertexAttribPointer gl pos dim vt_float false 0 0
      enableVertexAttribArray gl pos

update:: WebGLRenderingContext -> WebGLProgram -> Matrix M4 -> Effect Unit
update gl prog m = do
  lookAtPos <- getUniformLocation gl prog "lookAt"
  let (ColorCube cs ps) = colorCube
  case lookAtPos of
    Nothing -> debugE "Can't get uniform location"
    Just loc -> do
      uniformMat4f gl loc m
      drawArrays gl dm_triangles 0 $ length ps

mkLookAtDemo = template vertex fragment draw update RH.do
  eye /\ ele_eye <- useVector3 0.0   0.0    0.0
                              (-1.0) (-1.0) (-1.0)
                              1.0     1.0   1.0
                              0.05
                              "Eye"
  up /\ up_ele <- useVector3 0.0 1.0 0.0
                              (-1.0) (-1.0) (-1.0)
                              1.0     1.0   1.0
                              0.05
                              "Up"
  at /\ at_ele <- useVector3 0.0 0.0 0.0
                              (-1.0) (-1.0) (-1.0)
                              1.0     1.0   1.0
                              0.05
                              "At"
  pure $ (lookAt eye at up) /\ [div {
    children: [ele_eye,up_ele,at_ele],
    style: (css {
      display: "flex"
    })
  }]
