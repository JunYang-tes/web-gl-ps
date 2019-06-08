module Demos.Rotate where

import Controls
import Data.Matrix.Transform
import Data.Maybe
import Data.Nullable
import Data.Tuple.Nested
import Demos.Template
import Prelude
import WebGL
import WebGL.React

import Control.Monad.Trans.Class (lift)
import Data.Array (unsafeIndex)
import Data.Array.ST (empty, push, run)
import Data.Either (Either(..))
import Data.Nullable (null, Nullable)
import Data.Tuple (Tuple)
import Data.Vector (V4(..), Vector(..), flattenV, vec4)
import Debug (debug, debugE, debugEWithTag)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as D
import React.Basic.Hooks as RH

vertices :: Array (Vector V4)
vertices = [
  vec4 (-0.5) (-0.5) 0.5 1.0,
  vec4 (-0.5) ( 0.5) 0.5 1.0,
  vec4 ( 0.5) ( 0.5) 0.5 1.0,
  vec4 ( 0.5) (-0.5) 0.5 1.0,
  vec4 (-0.5) (-0.5) (-0.5) 1.0,
  vec4 (-0.5) ( 0.5) (-0.5) 1.0,
  vec4 ( 0.5) ( 0.5) (-0.5) 1.0,
  vec4 ( 0.5) (-0.5) (-0.5) 1.0
]
colorList:: Array (Vector V4)
colorList = [
  vec4 0.0 0.0 0.0 1.0, -- black
  vec4 1.0 0.0 0.0 1.0,
  vec4 1.0 1.0 0.0 1.0,
  vec4 0.0 1.0 0.0 1.0,
  vec4 0.0 0.0 1.0 1.0,
  vec4 1.0 0.0 1.0 1.0,
  vec4 0.0 1.0 1.0 1.0,
  vec4 1.0 1.0 1.0 1.0
]
type Color =  (Vector V4)
type Vertex = (Vector V4)
data ColorCube = ColorCube (Array Color) (Array Vertex)

colorCube :: ColorCube
colorCube = ColorCube (colors) (points)
  where
    colors :: Array Color
    colors =  color colorList 1
           <> color colorList 2
           <> color colorList 3
           <> color colorList 6
           <> color colorList 4
           <> color colorList 5
    points :: Array Vertex
    points = quad vertices 1 0 3 2
           <> quad vertices 2 3 7 6
           <> quad vertices 3 0 4 7
           <> quad vertices 6 5 1 2
           <> quad vertices 4 5 6 7
           <> quad vertices 5 4 0 1
    
    quad :: forall a. Array a -> Int -> Int -> Int -> Int -> Array a
    quad src a b c d = run (do
        arr <-empty
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src b ) arr
        _ <- push ( unsafePartial $ unsafeIndex src c ) arr
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src c ) arr
        _ <- push ( unsafePartial $ unsafeIndex src d ) arr
        pure arr
      )
    color :: forall a. Array a -> Int -> Array a
    color src a = run (do
        arr <- empty
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        _ <- push ( unsafePartial $ unsafeIndex src a ) arr
        pure arr
      )


vertex :: String
vertex = """
attribute vec4 vPosition;
attribute vec4 vColor;
varying   vec4 fColor;

void main() {
  fColor = vColor;
  gl_Position = vPosition;
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

draw:: WebGLRenderingContext -> WebGLProgram ->(Number /\ Number /\ Number)-> Effect Unit
draw gl prog (rx /\ ry /\ rz) = do
  gl `enable` enable_depth_test
  vertexBuffer <- createBuffer gl
  colorBuffer  <- createBuffer gl
  case (vertexBuffer /\ colorBuffer)  of
    (Right buf) /\ (Right colorBuf)-> do
      bindBuffer gl bt_array_buffer buf
      let (ColorCube cs ps) = colorCube
      (bufferData gl bt_array_buffer $ flattenV $ rotate rx ry rz ps) usage_static_draw
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
      drawArrays gl dm_triangles 0 36
      pure unit
    _ -> debugE "Can't create buffer"

rotateX :: Number-> Array (Vector V4) -> Array (Vector V4)
rotateX degree vs = let r = rotateYM4 degree in
  map (\v -> trans r v) vs

rotate:: Number -> Number -> Number -> Array (Vector V4) -> Array (Vector V4)
rotate rx ry rz vs = let m = (rotateXM4 rx) * (rotateYM4 ry) * (rotateZM4 rz)  in
  map (\v -> trans m v) vs

mkRotate :: RH.CreateComponent {}
mkRotate = template vertex fragment draw draw (RH.do
  rx /\ rx_ele <- useSlider 0.0 0.0 360.0 1.0 "X"
  ry /\ ry_ele <- useSlider 0.0 0.0 360.0 1.0 "Y"
  rz /\ rz_ele <- useSlider 0.0 0.0 360.0 1.0 "Z"
  pure ((rx /\ ry /\ rz) /\ [rx_ele,ry_ele,rz_ele])
)

