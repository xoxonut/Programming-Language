import Data.List

--1.(a)
countNeg :: [Int] -> Int
countNeg xs = length ([ 1 | x <- xs,x<0])
                
--1.(b)
raise :: Int -> Int -> Int 
raise x n = product([x | _ <- [1..n]])

--1.(c)
pascal :: Int -> [Int]
pascal 1 = [1]
pascal x = [1]++[ y | y <- pairs (pascal (x-1)) ]++[1]

pairs :: [Int] -> [Int]
pairs xs = [ a+b |(a,b) <- zip (init xs)  (tail xs)]

--1.(d)
search :: String -> Char -> [Int]
search xs y = [ b | (a,b) <- zip xs [0..(length xs)] , a==y]

--2
segments :: Eq a => [a] -> [[a]]
segments [] = [[]]
segments xs = filter (/=[]) [a | i <- inits xs , a <- tails i ]

--3
isort :: [(Int, Int)] -> [(Int, Int)]
isort [] = []
isort((x1,x2):xs) = insert (x1,x2) (isort xs)
  where
    insert (y1,y2) [] = [(y1,y2)]
    insert (y1,y2) ((z1,z2):zs) 
      | comparePair (y1,y2) (z1,z2) = ((y1,y2):(z1,z2):zs)
      | not (comparePair (y1,y2) (z1,z2))= ((z1,z2):(insert (y1,y2) zs))

isort2 :: ((Int, Int) -> (Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
isort2 _ [] = []
isort2 f ((x1,x2):xs) = insert (x1,x2) (isort xs)
  where
    insert (y1,y2) [] = [(y1,y2)]
    insert (y1,y2) ((z1,z2):zs) 
      | f (y1,y2) (z1,z2) = ((y1,y2):(z1,z2):zs)
      | not (f(y1,y2) (z1,z2))= ((z1,z2):(insert (y1,y2) zs))

comparePair :: (Int, Int) -> (Int, Int) -> Bool
comparePair (x1,x2) (y1,y2) 
  | x1 < y1 = True
  | x1 > y1 = False
  | x2 < y2 = True
  | x2 >= y2 = False
--4.(a)
q1f1a :: [Int] -> [Int]
q1f1a xs = map (*3) (filter (\y -> not ( y<3 || y>10 ) ) xs)
      
--4.(b)
q1f1b :: [Int] -> [Int]
q1f1b xs = [ x*3 | x <- xs , not ( x<3 || x>10 ) ]

--4.(c)
compre :: [a]->(a->b)->(a->Bool)->[b]
compre xs f p = map f ( filter p xs )

--5.
subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = [ zs | ys <- subsets xs , zs <- [x:ys,ys]]  

main = do
    pretest "countNeg [1, -1, 3, -5]" $ countNeg [1, -1, 3, -5]
    pretest "raise 2 4" $ raise 2 4
    pretest "pascal 5" $ pascal 5
    pretest "search 'NCCU is a good university' 'C'" $ search "NCCU is a good university" 'C'
    pretest "search 'NCCU is a good university' 'U'" $ search "NCCU is a good university" 'U'
    pretest "segments" $ segments [1,2,3]
    pretest "isort [(5,4),(3,2),(1,5),(2,6)]" $ isort [(5,4),(3,2),(1,5),(2,6)]
    pretest "q1f1a [1, 5, 15, -4, 7]" $ q1f1a [1, 5, 15, -4, 7]
    pretest "q1f1b [1, 5, 15, -4, 7]" $ q1f1b [1, 5, 15, -4, 7]
    pretest "compre [(-9)..7] (*3) odd" $ compre [(-9)..7] (*3) odd
    pretest "subsets [1, 2, 3]" $ subsets [1, 2, 3]
    where
    pretest p a = putStrLn $ p ++ " = " ++ show a
