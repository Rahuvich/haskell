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
    | otherwise = 


