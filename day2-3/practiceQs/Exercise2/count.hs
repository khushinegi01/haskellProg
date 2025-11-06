count :: [a] -> Int
count [] = 0
count (_:xs) = 1 + count xs

main = do 
    print(count [4,8,3,5,2])