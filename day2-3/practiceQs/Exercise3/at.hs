at :: [a] -> Int -> a
at [] _ = error "Index out of range"
at (x:xs) i
  | i < 0     = error "Negative index"
  | i == 0    = x
  | otherwise = at xs (i - 1)

main = do
    print(at [5,7,1,0,4] 2)