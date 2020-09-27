factorial :: Integer -> Integer
-- factorial retorna el factorial d'un nombre natural donat
factorial 0 = 1
factorial n = n * factorial (n - 1)

doblar x = x * 2

absValue :: Int -> Int
absValue x
  | x >= 0 = x
  | otherwise = (- x)

power :: Int -> Int -> Int
power _ 0 = 1
power x p = x ^ p

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = not (hasDivisors x 2)

hasDivisors :: Int -> Int -> Bool
hasDivisors x n
  | n * n > x = False
  | mod x n == 0 = True
  | otherwise = hasDivisors x (n + 1)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n -1) + slowFib (n -2)

quickFib :: Int -> Int
quickFib n = fst $ fibonacci n

fibonacci :: Int -> (Int, Int)
fibonacci 0 = (0, 1)
fibonacci 1 = (1, 1)
fibonacci n = (seg, res)
  where
    x = fibonacci $ n - 1
    seg = snd x
    res = (fst x) + (snd x)

myLength :: [Int] -> Int
myLength n
  | n == [] = 0
  | otherwise = 1 + myLength (tail n)

myMaximum :: [Int] -> Int
myMaximum array = myMaximumRecursive (tail array) (head array)

myMaximumRecursive :: [Int] -> Int -> Int
myMaximumRecursive array max
  | array == [] = max
  | head array <= max = myMaximumRecursive (tail array) max
  | head array > max = myMaximumRecursive (tail array) (head array)

average :: [Int] -> Float
average array
  | array == [] = 0
  | otherwise = (fromIntegral (mySum array)) / (fromIntegral (myLength array))

mySum :: [Int] -> Int
mySum array
  | array == [] = 0
  | otherwise = (head array) + mySum (tail array)

buildPalindrome :: [Int] -> [Int]
buildPalindrome array = (reverse array) ++ array

remove :: [Int] -> [Int] -> [Int]
remove arrayX arrayY
  | arrayY == [] = arrayX
  | elem (head arrayY) arrayX = remove (removeElem arrayX (head arrayY) 0) (tail arrayY)
  | otherwise = remove arrayX (tail arrayY)

removeElem :: [Int] -> Int -> Int -> [Int]
removeElem array num index
  | not (elem num array) || index >= myLength array = array
  | num == (array !! index) = removeElem (take index array ++ (drop (index + 1) array)) num (index + 1)
  | otherwise = removeElem array num (index + 1)

flatten :: [[Int]] -> [Int]
flatten matrix = concat matrix

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens array = ((evens array 0), (odds array 0))

odds :: [Int] -> Int -> [Int]
odds array index
  | index >= myLength array = array
  | array == [] = []
  | mod (array !! index) 2 /= 0 = odds (take index array ++ (drop (index + 1) array)) (index)
  | otherwise = odds array (index + 1)

evens :: [Int] -> Int -> [Int]
evens array index
  | index >= myLength array = array
  | array == [] = []
  | mod (array !! index) 2 == 0 = evens (take index array ++ (drop (index + 1) array)) (index)
  | otherwise = evens array (index + 1)
