module WebGL.React where
import Prelude
import Prim.Row

import Data.DateTime.Instant (unInstant)
import Effect.Now (now)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Debug (debug,debugE, debugEWithTag)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM (canvas, css)
import React.Basic.DOM as R
import React.Basic.DOM.Components.Ref (selectorRef)
import React.Basic.Hooks (CreateComponent, JSX, component, element, useRef)
import React.Basic.Hooks as RH
import Record (merge)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLCanvasElement (fromElement)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import WebGL (FragmentShader, VertexShader
  ,useProgram
  ,WebGLProgram
  ,WebGLRenderingContext, createWebGL,createProgram)
type GLHandler = WebGLRenderingContext -> Effect Unit
type WebGLProps a = (
  width:: Int,
  height:: Int,
  onWebGLCreated:: GLHandler
  | a
)
type WebGLProps' = WebGLProps ()

mkWebGL
  :: forall props props_
  . Union props WebGLProps' (WebGLProps props_)
  => Nub (WebGLProps props_) WebGLProps'
  => CreateComponent (Record props)
  -- :: CreateComponent (Record WebGLProps)
mkWebGL = do
  component "WebGL" renderComp
  where
    renderComp props  = let p = merge props { width: 500
                                 ,height:500
                                 ,onWebGLCreated: (debugEWithTag "No onWebGLCreated provided":: WebGLRenderingContext -> Effect Unit)
                      } in RH.do
                        let id = show $ unInstant $ unsafePerformEffect $ now
                        RH.useEffect unit (do
                          doc <- document =<< window
                          el <- getElementById id $ toNonElementParentNode doc
                          case fromElement =<< el of
                            Nothing -> debugEWithTag "Not found" id
                            Just canvas -> do
                              webgl <- createWebGL canvas
                              p.onWebGLCreated webgl

                          pure $ debugE "unmount"
                          )
                        pure $ R.canvas { width: show p.width <> "px"
                                          ,height: show p.height <>"px"
                                          ,id: show $ unInstant $ unsafePerformEffect $ now
                                          ,style: css { border: "1px solid black"}
                        }

type ProgramReady = WebGLRenderingContext ->
  WebGLProgram -> Effect Unit
type WebGLWithShadersProps a = (WebGLProps (
  vertex:: VertexShader,
  fragment:: FragmentShader,
  onReady:: ProgramReady 
  | a
))
type WebGLWithShadersProps' = WebGLWithShadersProps ()

mkWebGLWithShaders
  :: forall props props_
  . Union props WebGLWithShadersProps' (WebGLWithShadersProps props_)
  => Nub (WebGLWithShadersProps props_) WebGLWithShadersProps'
  => CreateComponent (Record props)
mkWebGLWithShaders = do
  webgl_comp <- mkWebGL
  component "WebGL-Shaders" (renderComp webgl_comp)
  where
    renderComp webgl props = 
      let p = merge props { width: 500
                          ,height: 500
                          ,onWebGLCreated: (debugEWithTag "":: GLHandler) 
                          ,vertex: "void main(){}"
                          ,fragment:"void main(){}"
                          ,onReady: (\gl prog -> do
                            debugEWithTag "No onReady provided: GL is" gl
                            debugEWithTag "No onReady provided: Program is" prog
                            pure unit
                            ):: ProgramReady
                          } in 
      let p' = p { onWebGLCreated = handleGLCreated p} in RH.do 
        pure (element webgl { width: p.width ,height: p.height, onWebGLCreated: p'.onWebGLCreated })
    handleGLCreated 
      :: Record WebGLWithShadersProps'->WebGLRenderingContext  -> Effect Unit
    handleGLCreated p gl = do
      prog <- createProgram gl p.vertex p.fragment
      case prog of
        (Left err)   -> debugEWithTag "Failed to create program" err
        (Right prog) -> do
          useProgram gl prog
          p.onReady gl prog
          pure unit
