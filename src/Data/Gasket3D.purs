module Data.Gasket3D where 
import Data.Tuple
import Data.Tuple.Nested
import Data.Vector
import Prelude

import Data.Array (unsafeIndex)
import Debug (debug)
import Partial.Unsafe (unsafePartial)

type Colors = Array (Vector V3)
type Points = Array (Vector V3)

gasket3D ::  Vector V3 
  -> Vector V3 
  -> Vector V3 
  -> Vector V3 
  -> Int
  -> (Points /\ Colors)
gasket3D a b c d level =
  if level <=0 then ([]/\[]) else go a b c d 0
  where
    baseColors = [
      vec3 1.0 0.0 0.0,
      vec3 0.0 1.0 0.0,
      vec3 0.0 0.0 1.0,
      vec3 0.0 0.0 0.0
    ]
    -- trangle :: Vector V3 -> Vector V3 -> Vector V3 -> Int -> (Colors /\ Points)
    trangle p1 p2 p3 color = unsafePartial $
      [p1,p2,p3] /\ [unsafeIndex baseColors color ,unsafeIndex baseColors color ,unsafeIndex baseColors color]
      -- map ((flip  unsafeIndex) color) [baseColors,baseColors,baseColors]

    tetra p1 p2 p3 p4 
      =  trangle p1 p2 p4 0
      <> trangle p1 p3 p4 1
      <> trangle p1 p2 p3 2
      <> trangle p2 p3 p4 3
    
    go p1 p2 p3 p4 l 
      | l == level = tetra p1 p2 p3 p4
      | otherwise  = 
        let m12 = mix 0.5 p1 p2 in
        let m14 = mix 0.5 p1 p4 in
        let m13 = mix 0.5 p1 p3 in
        let m24 = mix 0.5 p2 p4 in
        let m23 = mix 0.5 p2 p3 in
        let m43 = mix 0.5 p4 p3 in
        let sl  = l + 1 in
        go p1 m12 m13 m14 sl <>
        go m12 p2 m23 m24 sl <>
        go m13 m23 p3 m43 sl <>
        go m14 m24 m43 p4 sl 

divided3 :: (Points /\ Colors)
divided3 = gasket3D 
                (vec3  0.0000   0.0000  (-1.0000) )
                (vec3  0.0000   0.9428   0.3333 ) 
                (vec3 (-0.8165)  (-0.4714)   0.3333 ) 
                (vec3  0.8165  (-0.4714)   0.3333 ) 
                3    