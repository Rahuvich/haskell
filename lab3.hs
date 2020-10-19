myMap fn list = [fn x | x <- list]

myFilter fn list = [x | x <- list, fn x]

myZipWith fn xs ys = [fn x y | x <- xs, y <- ys]