final :: [a] -> a
final [] = error "Empty list has no final element"
final [x] = x
final (_:xs) = final xs

main = do 
    print(final [3,4,2,7])