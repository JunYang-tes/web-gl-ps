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
  ,len
  ,normal
  ,mix
  ,class VectorDim
  ,getDim
  ,toArray
  ,flattenV
  ,unsafeNormal
  ,cross3
  ,extend
  ,unsafeExtend
  -- ,crose
) where
import Prelude

import Data.Array (concat, foldl, length, zipWith)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Semiring (class Semiring)
import Data.Show (class Show, show)
import Math (sqrt)
import Partial.Unsafe (unsafePartial)

data Vector o = Vector (Array Number) o

class VectorDim o where
  getDim:: o -> Int


toArray:: forall a. Vector a -> Array Number
toArray (Vector arr _) = arr

class VectorOps o where
  dot:: Vector o -> Vector o -> Number
  scale:: Number -> Vector o -> Vector o
  len:: Vector o -> Number
  normal:: Vector o -> Maybe( Vector o)
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
derive instance v2Eq :: Eq V2
derive instance vec2Eq :: Eq (Vector V2)
derive instance v3Eq :: Eq V3
derive instance vec3Eq :: Eq (Vector V3)
derive instance v4Eq :: Eq V4
derive instance vec4Eq :: Eq (Vector V4)

instance v2Dim :: VectorDim V2 where
  getDim _ = 2

instance v3Dim :: VectorDim V3 where
  getDim _ = 3

instance v4Dim :: VectorDim V4 where
  getDim _ = 4

instance showVector :: Show (Vector o) where
  show (Vector arr _) ="vec" <> show (length arr) <> show arr

vec2 a b = Vector [a,b] V2
vec3 a b c = Vector [a,b,c] V3
vec4 a b c d = Vector [a,b,c,d] V4

op:: forall d. (Number -> Number -> Number) -> d -> Vector d -> Vector d -> Vector d
op f d (Vector a _) (Vector b _) = Vector (zipWith f a b) d

fadd = op (+)
fmul = op (*)
fsub = op (-)

fdot:: forall d. d -> Vector d -> Vector d -> Number
fdot d (Vector a _) (Vector b _) = foldl (\r n -> r+n) 0.0 $ zipWith (*) a b

fscale :: forall d. d -> Number -> Vector d -> Vector d
fscale d s (Vector a _) = (Vector (map (\v -> s * v) a) d)

flen :: forall d. d -> Vector d -> Number
flen _ (Vector arr _) = (sqrt $ sum $ map (\i -> i*i) arr)

fnormal :: forall d. d -> Vector d -> Maybe(Vector d)
fnormal d v = let l = flen d v in
  case l of
    0.0 -> Nothing
    _ -> Just $ fscale d (1.0 / l) v

unsafeNormal :: forall d. Partial => d -> Vector d -> Vector d
unsafeNormal d v = let l = flen d v in
  fscale d (1.0 / l) v


instance v2Semiring:: Semiring (Vector V2) where
  add = fadd V2
  mul = fmul V2
  one = vec2 1.0 1.0
  zero = vec2 0.0 0.0

instance v2Ring :: Ring (Vector V2) where
  sub = fsub V2

instance v3Semiring :: Semiring (Vector V3) where
  add = fadd V3
  mul = fmul V3
  one = vec3 1.0 1.0 1.0
  zero = vec3 0.0 0.0 0.0

instance v3Ring :: Ring (Vector V3) where
  sub = fsub V3



instance v2Ops :: VectorOps V2 where
  dot = fdot V2
  scale = fscale V2
  len = flen V2
  normal = fnormal V2

instance v3Ops :: VectorOps V3 where
  dot = fdot V3
  scale = fscale V3
  len = flen V3
  normal = fnormal V3

instance v4Ops :: VectorOps V4 where
  dot = fdot V4
  scale = fscale V4
  len = flen V4
  normal = fnormal V4

  -- crose a b = a

flattenV :: forall a.Array (Vector a) -> Array Number
flattenV vecs = concat $ map ex vecs
  where
    ex (Vector arr _) = arr

cross3 :: Vector V3 -> Vector V3 -> Vector V3
cross3 a b = unsafePartial $ cross a b 
  where
    cross :: Partial => Vector V3 -> Vector V3 -> Vector V3
    cross (Vector [u0,u1,u2] _) (Vector [v0,v1,v2] _) =
      vec3 (u1*v2 - u2*v1)
           (u2*v0 - u0*v2)
           (u0*v1 - u1*v0)

extend:: forall from to.VectorDim from => VectorDim to =>
  from -> to -> Vector from -> Number -> Maybe(Vector to)
extend f t (Vector arr _) ele =if getDim f == (getDim t)-1 then
    Just $ Vector (arr<>[ele]) t
  else
    Nothing

unsafeExtend :: forall from to. from -> to -> Vector from -> Number -> Vector to
unsafeExtend f t (Vector arr _) ele = Vector (arr <> [ele]) t