module Data.Matrix(mat2
,Matrix
,M2(..)
,M3(..)
,M4(..)
,mat4
,mat3
,transpose
,rows
,det
,invert
,class MatrixOps
,class MatrixOrder
,getOrder
,subMat
,fdet
,flattenM
) where
import Prelude

import Control.Monad.ST (run, for)
import Control.Monad.ST (run, for)
import Data.Array (concat, foldl, index, length, slice, snoc, unsafeIndex, zipWith)
import Data.Array.ST (withArray)
import Data.Array.ST (withArray)
import Data.Array.ST.Partial (poke, peek)
import Data.ArrayEx (filterp)
import Data.Maybe (Maybe(..))
import Debug (debug, debugc)
import Partial.Unsafe (unsafePartial)

data Matrix m = Matrix (Array Number) m
data M2 = M2
data M3 = M3
data M4 = M4


class MatrixOrder a where
  getOrder:: a -> Int

class (MatrixOrder m) <= MatrixOps m where
  transpose:: Matrix m -> Matrix m
  rows:: Matrix m -> Array (Array Number)
  det:: Matrix m -> Number
  invert:: Matrix m ->Maybe( Matrix m)


instance m2Order :: MatrixOrder M2 where
  getOrder _ = 2


instance m3Order :: MatrixOrder M3 where
  getOrder _ = 3

instance m4Order :: MatrixOrder M4 where
  getOrder _ = 4

showMatrix:: forall o. (MatrixOrder o) => Matrix o -> String
showMatrix (Matrix a o) = fmtMatrix a (getOrder o) "" 0
  where
    fmtMatrix:: Array Number -> Int -> String -> Int -> String
    fmtMatrix a o out ind= 
      if ind == length a
        then out
        else
            (if (ind `mod` o) == 0 then "\n" else " , ")
            <> case (index a ind) of
              (Just v) -> show v 
              _ -> "Never goes here! if did it, bug!"
            <> fmtMatrix a o out (ind + 1)


instance showMatrix2 :: Show (Matrix M2) where
  show = showMatrix

instance showMatrix3 :: Show (Matrix M3) where
  show = showMatrix

instance showMatrix4 :: Show (Matrix M4) where
  show = showMatrix


mat2:: Number -> Number -> Number -> Number -> Matrix M2
mat2 a b 
     c d = Matrix [a,b,c,d] M2

mat3 :: Number -> Number -> Number ->
        Number -> Number -> Number ->
        Number -> Number -> Number ->
        Matrix M3
mat3 a b c
     d e f
     g h i
     = Matrix [a,b,c,d,e,f,g,h,i] M3

mat4 :: Number -> Number -> Number -> Number ->
        Number -> Number -> Number -> Number ->
        Number -> Number -> Number -> Number ->
        Number -> Number -> Number -> Number ->
        Matrix M4
mat4 a b c d
     e f g h
     i j k l
     m n o p
     = Matrix [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] M4

fadd:: forall m. m -> Matrix m -> Matrix m -> Matrix m
fadd m (Matrix a _) (Matrix b _) = (Matrix (zipWith (+) a b) m)

frows:: forall m. (MatrixOrder m) => m -> Matrix m -> Array (Array Number)
frows m (Matrix a _ ) = let o = getOrder m in
  let len = o * o in
    toRows len o 0 []
    where
      toRows:: Int -> Int -> Int -> Array (Array Number) -> Array (Array Number)
      toRows len o ind result = if ind >= len 
        then result
        else let newRow = slice ind (ind + o) a in
          toRows len o (ind + o) (snoc result newRow)


{-
funtion mul (a,b) {
  const rs = rows(a)
  const cs = rows(transpose(b))
  const result = []
  for(let r of rs) {
    for(let c of cs) {
      result.push(
        zipWith((a,b)=>a*b,r,c)
          .reduce((r,n)=>r+n)
      )
    }
  }
  return result
}
-}
fmul:: forall m. MatrixOrder m => 
                 MatrixOps m => 
                 m -> Matrix m -> Matrix m ->  Matrix m
fmul m a b = (Matrix mul m)
  where
    mul = let rs = rows a in
    let cs = rows $ transpose b in
    do
      r <- rs
      c <- cs
      [ foldl (\r n-> r+n) 0.0 $ zipWith (*) r c ]



{-
function trans (m,order) {
  let t = []
  for(let i =0;i<order;i++) {
    let row = (i / order) | 0
    let col = (i % order)
    t[col * order + row] = m[i]
  }
  return t
}
-}

ftrans:: forall o. (MatrixOrder o) => o -> Matrix o -> Matrix o
ftrans o (Matrix a _) = (Matrix (transpose a (getOrder o)) o)
  where
    transpose:: Array Number -> Int -> Array Number
    transpose a o1 = 
      run (withArray (\arr->do
        for 0 o1 (\r ->
          for 0 o1 (\c ->
            unsafePartial $ poke (c*o1+r) (unsafeIndex a (r*o1+c)) arr
          )
        )
        pure arr
        ) a)

instance semiringM2:: Semiring (Matrix M2) where
  add = fadd M2
  mul = fmul M2
  zero = mat2 0.0 0.0 0.0 0.0
  one = mat2 1.0 0.0 0.0 1.0

instance semiringM4 :: Semiring (Matrix M4) where
  add = fadd M4
  mul = fmul M4
  zero = mat4 0.0 0.0 0.0 0.0
              0.0 0.0 0.0 0.0
              0.0 0.0 0.0 0.0
              0.0 0.0 0.0 0.0
  one  = mat4 1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              0.0 0.0 0.0 1.0

instance m2Math:: MatrixOps M2 where
  transpose = ftrans M2
  rows = frows M2
  det (Matrix [a,b,c,d] M2) = a*d - c*b
  det (Matrix _ _) = -1.0 -- Impossible
  invert m@(Matrix [a,b,c,d] M2) = let dt = det m in
    case dt of
      0.0 -> Nothing
      _ -> let _ =  debug "det" dt in Just (mat2 (
          (d)/dt) ((-b)/dt) 
          ((-c)/dt) (a/dt)
        )
  invert _ = Nothing


instance semiringM3:: Semiring (Matrix M3) where
  add = fadd M3
  mul = fmul M3
  zero = mat3 0.0 0.0 0.0 
              0.0 0.0 0.0
              0.0 0.0 0.0
  one = mat3 1.0 0.0 0.0 
             0.0 1.0 0.0
             0.0 0.0 1.0

instance m3Math:: MatrixOps M3 where
  transpose = ftrans M3
  rows = frows M3
  det = fdet M3
  invert m@(Matrix [
    a,b,c,
    d,e,f,
    g,h,i
  ] M3) = let dt = det m in
    case dt of
      0.0 -> Nothing
      _ -> Just (Matrix [
        (e*i - h*f) / dt, -(b*i -h*c) / dt, (b*f - c*e) / dt,
        (f*g -i*d) /  dt, -(c*g -i*a) / dt, (c*d - a*f) /dt,
        (d*h - g*e) / dt, -(a*h -g*b) / dt, (a*e - b*d) /dt
      ] M3)
  invert _ = Nothing



-- remove row r and col c get new matrix
subMatrixImp:: (Array Number) -> Int -> Int -> Int -> (Array Number)
subMatrixImp m r c order = filterp p m
  where
    p a ind = let r1 = ind / order in
      let c1 = ind `mod` order in
        (r /= r1 && c /= c1)

subMat:: forall a b. MatrixOrder a =>
  MatrixOrder b =>
  b -> Matrix a -> Int -> Int -> Maybe (Matrix b)
subMat b (Matrix arr a) r c = let order = getOrder a in
  if r>= order || c>=order then Nothing
  else 
    let orderB = getOrder b in 
      if orderB + 1 /= order then Nothing
      else Just (Matrix (subMatrixImp arr r c order) b)

-- ftrans:: forall o. (MatrixOrder o) => o -> Matrix o -> Matrix o
fdet:: forall a. MatrixOrder a => a -> Matrix a -> Number
fdet t (Matrix arr o) = calcDet arr (getOrder o)
  where
    calcDet:: (Array Number) -> Int -> Number
    calcDet arr order = case length arr of
      4 -> case arr of
            [a,b,c,d] -> a*d - c*b
            _ -> 0.0 -- never go here
      _ -> let firstRow = slice 0 order arr in
             foldl (\r n -> r+n) 0.0 $ (run (withArray (\row ->do
              for 0 order (\i ->
                (unsafePartial $ 
                poke i 
                  ((unsafePartial $ unsafeIndex firstRow i) 
                    * (calcDet (subMatrixImp arr 0 i order) (order-1))
                    * if i `mod` 2 == 0 then 1.0 else (-1.0)
                  )
                  row)
                )
              pure row
              ) firstRow))

flattenM :: forall m. (MatrixOps m)=> Matrix m -> Array Number
flattenM m = concat $ rows $ transpose m

instance m4Math :: MatrixOps M4 where
  transpose = ftrans M4
  rows = frows M4
  det = fdet M4
  invert _ = Nothing