module Demos.DrawArray where
import Controls (useRadioGroup)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toNullable)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Prelude (Unit, bind, discard, eq, negate, pure, unit, ($))
import WebGL (DrawMode, FragmentShader, VertexShader, WebGLProgram, WebGLRenderingContext, bindBuffer, bt_array_buffer, bufferData, createBuffer, dm_line_loop, dm_line_strip, dm_lines, dm_triangle_fan, dm_triangle_strip, dm_triangles, drawArrays, enableVertexAttribArray, getAttribLocation, two, usage_static_draw, vertexAttribPointer, vt_float)

import Data.Array.NonEmpty (NonEmptyArray, findIndex, unsafeIndex)
import Data.Nullable (Nullable, toNullable)
import Debug (debugE)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as D
import React.Basic.Hooks as RH
import Unsafe.Coerce (unsafeCoerce)
import WebGL.React (mkWebGLWithShaders)

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
    
update :: WebGLRenderingContext ->  DrawMode -> Effect Unit
update gl dm = do
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
mkDrawArrayDemo = do
  webgl <- mkWebGLWithShaders
  RH.component "DrawArray" \props -> RH.do
    ref <- RH.useRef (null:: Nullable WebGLRenderingContext)
    name /\ ele <-useRadioGroup drawModeNames
    ref_ <- RH.renderRefMaybe ref
    RH.useEffect name do
      case ref_ of
        Nothing -> pure unit
        Just (gl) -> do
          case findIndex (eq name) drawModeNames of
            Nothing -> pure unit
            Just ind -> update gl $ unsafePartial $ unsafeIndex drawModes ind
      pure $ pure unit
    pure $ D.div_ [
      ele
      ,RH.element webgl {
        vertex
        ,fragment
        ,onReady: (\gl prog -> do
          draw gl prog dm_lines
          RH.writeRef ref $ toNullable $ Just $ gl
        )
      }
    ]