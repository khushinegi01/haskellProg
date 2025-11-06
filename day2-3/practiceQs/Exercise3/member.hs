-- guard version 

member :: Int -> [Int] -> Bool
member _ [] = False
member n (x:xs)
  | n == x    = True
  | otherwise = member n xs

-- boolean version 

member' :: Int -> [Int] -> Bool
member' _ [] = False
member' n (x:xs) = (n == x) || member' n xs

main = do 
    print(member 4 [1..5])
    print(member' 4 [1..5])    