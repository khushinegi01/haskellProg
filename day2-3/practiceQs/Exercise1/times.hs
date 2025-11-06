

times :: [Int] -> Int
times [] = 1
times (x:xs) = x * times xs

main = do 
    print(times [1,2,3])