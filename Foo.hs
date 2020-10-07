import qualified Data.Set as Set
data Foo = Foo { left :: [Foo], right :: [Foo]}
	   deriving (Show)
fooleft   (Foo left right) = left
fooright (Foo left right) = right 
instance Ord Foo where
    (Foo [] _)<=(Foo _ []) = True
    x <= y = (judge1 (fooleft x) && judge2(fooright y)) where
        judge1 [] =True 
        judge1 (a:as) =judge1 as  && not  (y<=a)
        judge2 [] =True
        judge2 (b:bs) =judge2 bs && not  (b<=x)

instance Eq Foo where
    x == y = (x <= y) && (y <= x)

instance Num Foo where
    (+) (Foo[][]) (Foo[][]) = Foo[][]
    (+) x y =z where
	z=(Foo (Set.toAscList (Set.union (Set.fromList (map f1 (fooleft x))) (Set.fromList(map f2 (fooleft y))))) (Set.toAscList (Set.union (Set.fromList (map f1 (fooright x))) (Set.fromList(map f2 (fooright y)))))) where
	f1 a = a+y
	f2 b = b+x
               
