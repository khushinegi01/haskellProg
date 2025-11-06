pair :: [a] -> [b] -> [(a,b)]
pair [] _ = []
pair _ [] = []
pair (x:xs) (y:ys) = (x,y) : pair xs ys

main = do
    print(pair [1..10] ["a","b","c","d","e"])