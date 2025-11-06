concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate (x:xs) = append x (concatenate xs)


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys



main = do 
    print (concatenate [[9,4,5],[],[2,4],[6]])