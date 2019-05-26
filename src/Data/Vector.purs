module Data.Vector(
  vec2
  ,vec3
  ,Vector(..)
  ,V2(..)
  ,V3(..)
  ,V4(..)
  ,vec4
  ,class VectorOps
  ,dot
  ,(<.>)
  ,scale, (<**>)
  ,mix
  ,class VectorDim
  ,getDim
  ,getArray
  ,flattenV
  -- ,crose
) where
import Prelude

import Data.Array (concat, foldl, zipWith)
import Data.Semiring (class Semiring)
import Data.Show (class Show, show)

data Vector o = Vector (Array Number) o

class VectorDim o where
  getDim:: o -> Int


getArray:: forall a. Vector a -> Array Number
getArray (Vector arr _) = arr

class VectorOps o where
  dot:: Vector o -> Vector o -> Number
  scale:: Number -> Vector o -> Vector o
  -- mix s a b = s <**> a + (1 - s) <**> b

infixl 6 dot as <.>
infixl 6 scale as <**>

mix:: forall o. Semiring (Vector o) =>
  VectorOps o =>
  Number -> Vector o -> Vector o -> Vector o
mix s a b = (s <**> a) + ((1.0 - s) <**> b)

data V2 = V2
data V3 = V3
data V4 = V4

instance v2Dim :: VectorDim V2 where
  getDim _ = 2

instance v3Dim :: VectorDim V3 where
  getDim _ = 3

instance v4Dim :: VectorDim V4 where
  getDim _ = 4

instance showVector :: Show (Vector o) where
  show (Vector arr _) = show arr

vec2 a b = Vector [a,b] V2
vec3 a b c = Vector [a,b,c] V3
vec4 a b c d = Vector [a,b,c,d] V4

op:: forall d. (Number -> Number -> Number) -> d -> Vector d -> Vector d -> Vector d
op f d (Vector a _) (Vector b _) = Vector (zipWith f a b) d

fadd = op (+)
fmul = op (*)

fdot:: forall d. d -> Vector d -> Vector d -> Number
fdot d (Vector a _) (Vector b _) = foldl (\r n -> r+n) 0.0 $ zipWith (*) a b

fscale :: forall d. d -> Number -> Vector d -> Vector d
fscale d s (Vector a _) = (Vector (map (\v -> s * v) a) d)

instance v2Semiring:: Semiring (Vector V2) where
  add = fadd V2
  mul = fmul V2
  one = vec2 1.0 1.0
  zero = vec2 0.0 0.0

instance v2Ops :: VectorOps V2 where
  dot = fdot V2
  scale = fscale V2
  -- crose a b = a

flattenV :: forall a.Array (Vector a) -> Array Number
flattenV vecs = concat $ map ex vecs
  where
    ex (Vector arr _) = arr