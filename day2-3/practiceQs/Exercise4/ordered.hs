ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:y:zs) = (x <= y) && ordered (y:zs)

main = do 
    print (ordered [1..10])
    print (ordered [3,6,0,4])
    