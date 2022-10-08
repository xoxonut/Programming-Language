--1.(a)
maxl :: (Ord a) => [a]->a
maxl xs = foldr max (head xs) xs
--1.(b)
member :: (Eq a) => [a] -> a -> Bool
member xs s = foldr (||) False ( map ( == s ) xs )
--1.(c)
myMap :: (a->b)->[a]->[b]
myMap f  = foldr (\y  ys-> f(y):ys) [] 
--2.
occurrences :: [Char] -> [(Char,Int)]
occurrences xs = zip (remdup xs) (map (\y->elemOcc y xs) (remdup xs))
--helper
remdup :: [Char]->[Char]
remdup[]=[]
remdup(x:xs) = x : remdup( filter (/=x) xs )
elemOcc :: Char-> [Char]->Int
elemOcc x xs = length (filter (==x) xs)
--3.
data Tree a = Empty | Node  a (Tree a) (Tree a) deriving Show
bfs :: Tree a -> [a]
bfs x = atraverse [x]

atraverse :: [Tree a] -> [a]
atraverse [] =[]
atraverse ts = rootlabels ++ atraverse children
  where rootlabels = [ x | (Node x _ _ ) <- ts ]
        children = [ z | (Node _ x y)<-ts, z<-[x,y] ]
--4.(a)
data Proposition = Var String
                | F
                | T
                | Not Proposition
                | Proposition :|: Proposition --or
                | Proposition :&: Proposition
                deriving (Eq, Ord, Show)

isNorm :: Proposition -> Bool
isNorm (Var x) = True
isNorm (F) = True
isNorm (T) = True
isNorm (Not (Var x)) = True
isNorm (Not p) = False
isNorm (p :|: q) = isNorm(p)&&isNorm(q)
isNorm (p :&: q) = isNorm(p)&&isNorm(q)
--4.(b)
norm :: Proposition -> Proposition
norm p | isNorm p = p
norm (Not (Var x)) = Var x
norm (Not F) = T
norm (Not T) = F
norm (Not (p :|: q)) = norm (Not p) :&: norm (Not q)
norm (Not (p :&: q)) = norm (Not p) :|: norm (Not q)
norm (p :&:q) = norm(p) :&: norm(q)
norm (p :|:q) = norm(p) :|: norm(q)
norm (Not (Not p)) = norm p
--5.
data Edit = Change Char | Copy | Delete | Insert Char deriving (Eq, Show)

cost :: [Edit] -> Int
cost = length . filter (/=Copy)

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b = x
  | otherwise = b
       where
             b = best xs

transform:: String -> String -> [ Edit ]
transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y)
    | a==b = Copy : transform x y
    | otherwise = best [ Delete : (transform x (b:y)) , 
                         Insert b : (transform (a:x) (y)) , 
                         Change b :(transform x y) ]


main = do
  pretest "maxl [1,3,2,5,6,4]" $ maxl [1,3,2,5,6,4]
  pretest "member ['a','b','x','z'] 'b'" $ member ['a','b','x','z'] 'b'
  pretest "member [1,2,3] 4" $ member [1,2,3] 4
  pretest "member [1,2,3,4.0,5] 4" $member [1,2,3,4.0,5] 4
  pretest "myMap even [1,2,3,4,5,6]" $ myMap even [1,2,3,4,5,6]
  pretest "remdup"$remdup ['a','c','d','a','c'] 
  pretest "elemOcc"$elemOcc 's' ['a','c','d','a','c'] 
  pretest "occurrences ['a','c','d','a','c'] " $ occurrences ['a','c','d','a','c'] 
  pretest "bfs t = [1, 10, 17, 16, 14, 20]" $ bfs (Node 1 (Node 10 Empty (Node 16 Empty Empty)) (Node 17 (Node 14 Empty Empty) (Node 20 Empty Empty)))
  pretest "isNorm (Var p :&: Not (Var q)) " $ isNorm (Var "p" :&: Not (Var "q"))
  pretest "isNorm (Not (Var p :|: Var q))" $ isNorm (Not (Var "p" :&: Not (Var "q")))
  pretest "norm" $norm (Not (Not (Var "p")) :|: Not T)
  pretest "transform" $ transform "abcde" "bbc"
  where
  pretest p a = putStrLn $ p ++ " = " ++ show a