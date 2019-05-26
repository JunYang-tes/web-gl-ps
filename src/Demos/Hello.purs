module Demos.HelloGL where
import Prelude (Unit, bind, discard, negate, pure, show, unit, ($), (<>))
import React.Basic.DOM as D
import React.Basic.Hooks as RH
import Data.Tuple (Tuple)
import Data.Tuple.Nested((/\))
import Data.Nullable (Nullable, null, toNullable)
import Controls (useSlider)
import WebGL (FragmentShader, VertexShader, WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, clearColor, createBuffer, dm_line_strip, drawArrays, enableVertexAttribArray, getAttribLocation, two, usage_static_draw, vertexAttribPointer, vt_float)
import Effect (Effect)
import Data.Either (Either(..))
import Debug (debug, debugE)
import WebGL.React (ProgramReady, mkWebGLWithShaders)
import Data.Maybe (Maybe(..))

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

helloWebGL:: WebGLRenderingContext -> WebGLProgram -> Effect Unit
helloWebGL gl prog = do
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

updateGL:: WebGLRenderingContext -> WebGLProgram -> Number -> Number -> Effect Unit
updateGL gl prog y x = do
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
mkHelloGL = do
  webgl' <- mkWebGLWithShaders:: Effect (RH.ReactComponent (Record 
      (vertex::VertexShader
      ,fragment::FragmentShader
      ,onReady::ProgramReady
      )))
  RH.component "Hello" \props -> RH.do
    val /\ ele <- useSlider 1.0 (-1.0) 1.0 0.01 "Y"
    x /\ xele <- useSlider 1.0 (-1.0) 1.0 0.01 "X"
    ref <- RH.useRef (null :: Nullable (Tuple WebGLRenderingContext WebGLProgram) )
    r <- RH.renderRefMaybe ref
    RH.useEffect (show x <> show val) do
      pure case r of
        Nothing -> debugE "No ref yet"
        Just (gl /\ prog) -> updateGL gl prog val x
    let _ = debug "rerendre"

    pure $ D.div_ [
      ele
      ,xele
      ,RH.element webgl' {vertex
                          ,onReady: (\gl prog->do
                            helloWebGL gl prog
                            RH.writeRef ref $ toNullable $ Just $ gl /\ prog
                            )
                          ,fragment}
    ] 
    