remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove n (x:xs)
  | n == x    = remove n xs
  | otherwise = x : remove n xs


main = do 
    print(remove 2 [1,2,3,2,1])