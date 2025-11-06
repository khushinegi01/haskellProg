range :: Int -> Int -> [Int]
range n m
  | n > m     = []
  | otherwise = n : range (n + 1) m

main = do 
  print(range 4 9)