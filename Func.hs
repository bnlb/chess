module Func (
  funcAnd,
  funcOr
) where


mapAp :: [ a -> b -> c ] -> a -> b -> [ c ]
mapAp fs x y = map (\f -> f x y) fs


-- funcAnd [ (==), (>) ] 5 5 == False
-- funcAnd [ (==), (<=) ] 5 9 == True
funcAnd :: [ a -> b -> Bool ] -> a -> b -> Bool
funcAnd fs x y = and (mapAp fs x y)


-- funcOr [ (==), (>) ] 5 5 == True
-- funcOr [ (==), (>) ] 5 9 == False
funcOr :: [ a -> b -> Bool ] -> a -> b -> Bool
funcOr fs x y = or (mapAp fs x y)