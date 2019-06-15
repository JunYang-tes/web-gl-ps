module Demos.Gasket where
import Data.Tuple.Nested
import Demos.Template
import Prelude
import WebGL

import Controls (useSlider)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Gasket3D (gasket3D)
import Data.Int (floor)
import Data.Vector (flattenV, vec3)
import Debug (debugE, debugEWithTag)
import Effect (Effect)
import React.Basic.Hooks (ReactComponent)
import React.Basic.Hooks as RH

vertex :: String
vertex = """
attribute vec4 vPosition;
attribute vec4 vColor;
varying vec4 fColor;

void
main()
{
  gl_PointSize = 1.0;
  fColor = vColor;
  gl_Position = vPosition;
}
"""

fragment :: String
fragment = """ 
precision mediump float;
varying vec4 fColor;
void main() {
  gl_FragColor = fColor; 
}
"""

draw :: WebGLRenderingContext -> WebGLProgram -> Int -> Effect Unit
draw gl prog level = do
  gl `enable` enable_depth_test
  -- gl `clear` (mask_color_buffer_bit .|. mask_depth_buffer_bit`)
  let (ps /\ cs) = bimap flattenV flattenV $ gasket3D 
                      (vec3  0.0000   0.0000  (-1.0000) )
                      (vec3  0.0000   0.9428   0.3333 ) 
                      (vec3 (-0.8165)  (-0.4714)   0.3333 ) 
                      (vec3  0.8165  (-0.4714)   0.3333 ) 
                      level
  debugEWithTag "cs" cs
  debugEWithTag "ps" ps
  buffer1 <- createBuffer gl
  buffer2 <- createBuffer gl
  case (buffer1 /\ buffer2) of
    (Right buf) /\ (Right cbuf) -> do
      bindBuffer gl bt_array_buffer buf
      bufferData gl bt_array_buffer ps usage_static_draw
      pos <- getAttribLocation gl prog "vPosition"
      vertexAttribPointer gl pos three vt_float false 0 0
      enableVertexAttribArray gl pos
 
      bindBuffer gl bt_array_buffer cbuf
      bufferData gl bt_array_buffer cs usage_static_draw
      vColor <- getAttribLocation gl prog "vColor" 
      vertexAttribPointer gl vColor three vt_float false 0 0
      enableVertexAttribArray gl vColor

      drawArrays gl dm_triangles 0 $ (length ps)/3
    _  -> debugE "can't create buffer"

mkGasket :: Effect (ReactComponent (Record ()))
mkGasket = template vertex fragment draw draw (RH.do
  level /\ ele <- useSlider 1.0 1.0 5.0 1.0 "Level"
  pure $ ((floor level) /\ [ele])
)