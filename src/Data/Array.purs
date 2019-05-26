module Data.ArrayEx where
foreign import filterp :: forall a. 
  (a -> Int -> Boolean) -> Array a -> Array a