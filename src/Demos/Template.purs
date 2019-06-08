module Demos.Template where
import Data.Matrix.Transform
import Data.Nullable
import Data.Tuple
import Data.Tuple.Nested
import Effect
import Prelude
import WebGL
import WebGL.React
import Data.Maybe
import React.Basic.DOM as D
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as RH

type Draw a = WebGLRenderingContext -> WebGLProgram -> a -> Effect Unit  


template::forall hooks a. (Eq a) => VertexShader -> FragmentShader ->
  Draw a -> Draw a ->RH.Render Unit hooks (Tuple a (Array JSX)) ->
  RH.CreateComponent {}
template vertex fragment draw update controls = do
  webgl <- mkWebGLWithShaders :: Effect (
    RH.ReactComponent (Record
      (vertex:: VertexShader
      ,fragment:: FragmentShader
      ,onReady:: ProgramReady
      )
    )
  )
  RH.component "Demo" \p -> RH.do
    vals /\ es <- controls
    glRef <- RH.useRef (null::(Nullable (Tuple WebGLRenderingContext WebGLProgram)))
    glRef_ <-RH.renderRefMaybe glRef
    RH.useEffect vals do
      case glRef_ of
        Nothing -> pure unit
        Just (gl /\ prog) -> update gl prog vals
      pure $ pure unit
    pure $ D.div_ (es <> [
      RH.element webgl {
        vertex
        ,fragment
        ,onReady: (\gl prog -> do
          RH.writeRef glRef $ toNullable $ Just $ gl /\ prog
          draw gl prog vals
          pure unit)
      }
    ])