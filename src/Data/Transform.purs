module Data.Matrix.Transform(
  rotateXM4
  ,rotateXM3
  ,rotateYM4
  ,rotateZM4
  ,rotate
  ,unsafeRotate
  ,(|>>),trans
  ,class VectorTransform
  ,(|=>),transVectors
  ,scaleX
  ,scaleY
  ,scaleZ
  ,shearXY
  ,shearXZ
  ,shearYX
  ,shearYZ
) where

import Prelude

import Data.Array (foldl, zipWith)
import Data.Matrix (class MatrixOps, class MatrixOrder, M3(..), M4(..), Matrix, getOrder, i4, mat3, mat4, rows, transpose)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Vector (class VectorDim, V3(..), V4(..), Vector(..), getDim, normal)
import Math (cos, pi, sin, sqrt, tan)
import Partial.Unsafe (unsafePartial)

radians :: Number -> Number
radians a = a * pi / 180.0


{-
       y
       |
       |_____ x
      /
     /
    z
-}

translate :: Number -> Number -> Number -> Matrix M4
translate x y z = mat4 1.0 0.0 0.0   x
                       0.0 1.0 0.0   y
                       0.0 0.0 1.0   z
                       0.0 0.0 0.0 1.0

rotateXM4 :: Number -> Matrix M4
rotateXM4 a = let s = sin( radians a) in
              let c = cos(radians a) in mat4 
                1.0  0.0 0.0  0.0
                0.0  c   (-s) 0.0
                0.0  s   c    0.0
                0.0  0.0 0.0  1.0

scaleX :: Number -> Matrix M4
scaleX s = mat4 s   0.0 0.0 0.0
                0.0 1.0 0.0 0.0
                0.0 0.0 1.0 0.0
                0.0 0.0 0.0 1.0

scaleY :: Number -> Matrix M4
scaleY s = mat4 1.0 0.0 0.0 0.0
                0.0 s   0.0 0.0
                0.0 0.0 1.0 0.0
                0.0 0.0 0.0 1.0

scaleZ :: Number -> Matrix M4
scaleZ s = mat4 1.0 0.0 0.0 0.0
                0.0 1.0 0.0 0.0
                0.0 0.0 s   0.0
                0.0 0.0 0.0 1.0

rotateXM3 :: Number -> Matrix M3
rotateXM3 a = let s = sin( radians a) in
              let c = cos(radians a) in mat3
                1.0  0.0 0.0  
                0.0  c   (-s) 
                0.0  s   c    


rotateYM4 :: Number -> Matrix M4
rotateYM4 a = let s = sin (radians a) in
              let c = cos (radians a) in mat4
                c    0.0  (s)  0.0
                0.0  1.0  0.0  0.0
                (-s) 0.0  c    0.0  
                0.0  0.0  0.0  1.0

rotateYM3 :: Number -> Matrix M3
rotateYM3 a = let s = sin (radians a) in
              let c = cos (radians a) in mat3
                c   0.0  (-c) 
                0.0 1.0  0.0  
                s   0.0  s     

rotateZM3 :: Number -> Matrix M3
rotateZM3 a = let s = sin $ radians a in
              let c = cos $ radians a in mat3
                c     (-s)  0.0
                s     c     0.0
                0.0   0.0   1.0

rotateZM4 :: Number -> Matrix M4
rotateZM4 a = let s = sin $ radians a in
              let c = cos $ radians a in mat4
                c     (-s)  0.0 0.0
                s     c     0.0 0.0
                0.0   0.0   1.0 0.0
                0.0   0.0   0.0 1.0


rotate :: Vector V4 -> Vector V4 -> Number ->Maybe(Matrix M4)
rotate fixedPoint direction degree = case normal direction of
  Nothing -> Nothing
  Just normalized -> Just $ unsafePartial $ rotateImpl fixedPoint normalized degree
  where
    rotateImpl :: Partial => Vector V4 -> Vector V4 -> Number -> Matrix M4
    rotateImpl (Vector [a,b,c,_] _) (Vector [x,y,z,_] _) d = 
      let t = translate (-a) (-b) (-c) in
      let dx = sqrt (y*y+z*z) in
      
      let rx = mat4 1.0   0.0    0.0    0.0
                    0.0  (z/dx) (-y/dx) 0.0
                    0.0  (y/dx) (z/dx)  0.0
                    0.0   0.0    0.0    1.0 in
      let ry = mat4 dx  0.0  (-x)  0.0 
                    0.0 1.0  0.0   0.0
                    x   0.0  dx    0.0
                    0.0 0.0  0.0   1.0  in
      let t1 = translate a b c in
        t1 * (transpose rx) * (transpose ry)* (rotateZM4 d) * ry *rx* t

unsafeRotate:: Vector V4 -> Vector V4 -> Number -> Matrix M4  
unsafeRotate  fixedPoint direction degree = 
  case rotate fixedPoint direction degree of
    Just m  -> m
    Nothing -> i4

cot :: Number -> Number
cot a = 1.0 / (tan a)

shearXY :: Number -> Matrix M4
shearXY a = let t = cot $ radians a in
  mat4 1.0 t 0.0 0.0
       0.0   1.0 0.0 0.0
       0.0 0.0 1.0 0.0
       0.0 0.0 0.0 1.0

shearXZ :: Number -> Matrix M4
shearXZ a = let t = cot $ radians a in
  mat4 1.0 0.0   t 0.0
       0.0 1.0 0.0 0.0
       0.0 0.0 1.0 0.0
       0.0 0.0 0.0 1.0

shearYX :: Number -> Matrix M4
shearYX a = let t = cot $ radians a in
  mat4 1.0 0.0 0.0 0.0
       t   1.0 0.0 0.0
       0.0 0.0 1.0 0.0
       0.0 0.0 0.0 1.0


shearYZ :: Number -> Matrix M4
shearYZ a = let t = cot $ radians a in
  mat4 1.0 0.0 0.0 0.0
       0.0 1.0 t   0.0
       0.0 0.0 1.0 0.0
       0.0 0.0 0.0 1.0

shearZ :: Number -> Matrix M4
shearZ a = let t = cot $ radians a in
  mat4 1.0 0.0 0.0 0.0
       0.0 1.0 0.0 0.0
       0.0 t  1.0 0.0
       0.0 0.0 0.0 1.0

frotate :: (Vector V4 -> (Number /\ Number /\ Number)) -> Array (Vector V4) -> Array (Vector V4)
frotate f vs = vs
-- frotate f  = map (\v->
--               let (rx /\ ry /\ rz) = f v in
--                 rotateXM4 rx |>> rotateYM4 ry |>> rotateZM4 rz |>> v
--               )


class VectorTransform o m where
  trans:: Matrix m -> Vector o ->  Vector o

infixr 6 trans as |>>

transVectors:: forall d m. (VectorTransform d m)=>
  Matrix m -> Array (Vector d) -> Array (Vector d)
transVectors m = map (\v -> m |>> v)

infixr 6 transVectors as |=>



ftrans :: forall o m. (MatrixOrder m) => (VectorDim o) =>
  (MatrixOps m) =>
  o -> m -> Matrix m -> Vector o -> Vector o
ftrans o m mat v@(Vector arr d) = if getOrder m == getDim (o) then
    let rs = rows mat in
      (Vector (map (\r -> foldl (\r n->r+n) 0.0 $ zipWith (*) r arr) rs) d)
  else v

instance v3Transform :: VectorTransform V3 M3 where
  trans = ftrans V3 M3

instance v4Transform :: VectorTransform V4 M4 where
  trans = ftrans V4 M4
