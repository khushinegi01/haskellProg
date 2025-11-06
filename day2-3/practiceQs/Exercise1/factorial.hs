range :: Int -> Int -> [Int]
range n m
  | n > m     = []
  | otherwise = n : range (n + 1) m

times :: [Int] -> Int
times [] = 1
times (x:xs) = x * times xs

factorial :: Int -> Int
factorial n = times (range 1 n)


main = do 
  print(factorial 10)