eql :: [Int] -> [Int] -> Bool
eql xs ys = all (\x -> x == True) (zipWith (\x y -> x == y) xs ys) && length xs == length ys

prod :: [Int] -> Int
prod xs = foldl (*) 1 xs

prodOfEvens :: [Int] -> Int
prodOfEvens xs =
  prod
    (filter even xs)

powersOf2 :: [Int]
powersOf2 = (iterate (* 2) 1)

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = (foldl (+) 0 (zipWith (\x y -> x * y) xs ys))

flatten :: [[Int]] -> [Int]
flatten xs = foldl (\xs ys -> xs ++ ys) [] xs

myLength :: String -> Int
myLength s = foldl (\x y -> x + 1) 0 s

myReverse :: [Int] -> [Int]
myReverse xs = tail $ scanl (\x y -> y) 0 $ reverse xs

countInList :: [Int] -> Int -> Int
countInList xs y = length $ filter (\x -> x == True) $ map (\x -> x == y) xs

countIn :: [[Int]] -> Int -> [Int]
countIn xss y = map (\xs -> countInList xs y) xss

firstWord :: String -> String
firstWord s = takeWhile (\y -> y /= ' ') $ dropWhile (\x -> x == ' ') s
