-- example
power :: Int -> Int -> Int
power n 0 = 1
power n k = n * power n (k-1)

-- 1. (a)
power1 :: Int -> Int -> Int
power1 n k = product ( replicate k n ) 

-- 1. (b)
power2 :: Int -> Int -> Int 
power2 n k | odd k && k/=1 = n * power2 n (k-1)  
           | even k = power2 (n*n) (div k 2) 
           | k<=1 = n

-- 2.(a)
myButLast :: [a] -> a
myButLast(x:xs) | length xs == 1 = x
myButLast(_:xs) | otherwise = myButLast xs


-- 2.(b)
rev2 :: Eq a => [a] -> [a]
rev2 s@(x:xs) | (reverse s) == (xs++[x]) = reverse s  
              | otherwise = s

-- 3. (a)
tailUpto :: Int -> Int -> [Int] -> [Int]
tailUpto s e xs | s<=e = tailUpto s (e-1) ([e]++xs)
                | otherwise = xs

-- 3. (b)
tailFib :: Int -> Int -> Int -> Int
tailFib n a1 a2
  |n==0 = a2
  |otherwise = tailFib (n-1) a2 (a1+a2)


-- 4. (a)
palindrome :: [Int] -> Bool
palindrome xs 
  | reverse xs == xs = True
  | otherwise = False

-- 4. (b)
isPermutation :: [Int] -> [Int] -> Bool
isPermutation [] [] = True
isPermutation x y | length x /= length y = False
isPermutation (x:xs) y = isPermutation xs (removeOnce x y)

-- helper function for 4. (b)
removeOnce :: Int -> [Int] -> [Int]
removeOnce x [] = []
removeOnce x (y:ys) 
 | x/=y = y:removeOnce x ys
 | x==y = ys


main = do
    pretest "power 7 5" $ power 7 5
    pretest "power 3 7" $ power 3 7
    pretest "power1 7 5" $ power1 7 5
    pretest "power2 3 7" $ power2 3 7
    pretest "myButLast [1, 2, 3, 4]" $ myButLast [1, 2, 3, 4]
    pretest "myButLast ['a'..'z']" $ myButLast ['a'..'z']
    pretest "rev2 [1, 2]" $ rev2 [1, 2]
    pretest "rev2 [1, 2, 3]" $ rev2 [1, 2, 3]
    pretest "tailUpto 3 8 [1,2]" $ tailUpto 3 8 [1, 2]
    pretest "tailUpto 8 3 [1]" $ tailUpto 8 3 [1]
    pretest "tailFib 5 0 1" $ tailFib 5 0 1
    pretest "palindrome [1, 2, 2, 3, 3]" $ palindrome [1, 2, 2, 3, 3]
    pretest "palindrome [1, 2, 3, 2, 1]" $ palindrome [1, 2, 3, 2, 1]
    pretest "palindrome [3]" $ palindrome [3]
    pretest "palindrome []" $ palindrome []
    pretest "isPermutation [] []" $ isPermutation [] []
    pretest "isPermutation [1,2,1] [2,1,1]" $ isPermutation [1,2,1] [2,1,1]
    pretest "isPermutation [1,2,1] [2,1,2]" $ isPermutation [1,2,1] [2,1,2]
    pretest "isPermutation [1,2,1] [2,1,1,2]" $ isPermutation [1,2,1] [2,1,1,2]
    pretest "removeOnce 3 [1,3,5,3,4]" $ removeOnce 3 [1,3,5,3,4]
    pretest "removeOnce 5 [1,2,3,3,4]" $ removeOnce 5 [1,2,3,3,4]
    pretest "removeOnce 3 []" $  removeOnce 3 []
    where
    pretest p a = putStrLn $ p ++ " = " ++ show a