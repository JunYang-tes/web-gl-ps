module Test where


-- -- https://hgiasac.github.io/posts/2018-11-18-Record-Row-Type-and-Row-Polymorphism.html 
-- module Test where
-- import Prelude
-- import Prim.Row

-- import Effect (Effect, forE)
-- import Effect.Console (log)
-- import Record (merge)
-- import Data.Array.ST as AST
-- import Control.Monad.ST as ST

-- type ObjectRow = ( x :: Int, y :: String )
-- type OpenRow r = ( x :: Int, y :: String | r )

-- result :: 
--   forall r1 r2
--   . Union r1 ObjectRow  (OpenRow r2)
--   => Nub (OpenRow r2) ObjectRow
--   => Record r1 -> Record ObjectRow 
-- result r1 = merge r1 { x: 0, y: "" }

-- f :: Record ObjectRow -> String
-- f a = a.y

-- testVoid :: Unit -> Effect Unit
-- testVoid _ = do
--  log("hello test")
--  log("hello again")
--  forE 1 10 (\a -> do
--    log $ show a 
--    log $ show $ a * a
--  )

--  pure unit

-- test = AST.run (do
--       arr <- AST.empty
--       _<-AST.push 1 arr
--       pure arr)

-- -- test = let arrST = (AST.empty >>= (\arr -> const (pure arr) (AST.push 1 arr))) in AST.run arrST

-- arrST = do
--   arr <- AST.empty
--   _ <- AST.push 1 arr
--   pure arr

-- test2 = AST.run arrST

-- -- ????
-- -- test3 = AST.run $ do
-- --   arr <- AST.empty
-- --   _ <- AST.push 1 arr
-- --   pure arr
-- -- test4 = let arrST = do
-- --                       arr <- AST.empty
-- --                       _ <- AST.push 1 arr
-- --                       pure arr in AST.run arrST

-- test5 = AST.run (do
--       arr <- AST.empty
--       ST.for 1 10 (\i -> AST.push i arr)
--       pure arr)

-- test6 = AST.run (do
--       arr <- AST.empty
--       ST.for 1 10 (\i -> 
--         ST.for i 10 (\j -> AST.push (show i <> "," <> show j) arr)
--       )
--       pure arr)

-- test7 = ST.run (AST.withArray (\arr -> do
--   _ <- AST.push 10 arr
--   pure arr
-- ) [1,2,3])