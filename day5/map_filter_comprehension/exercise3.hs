-- Exercise 3: Zip & zipWith

-- (a) numbered: index elements starting from 1
numbered :: [a] -> [(Int,a)]
numbered xs = zip [1..] xs


-- (b) everyother: keep every other element starting with the first
-- version using numbered + filter/map
everyother :: [a] -> [a]
everyother xs = map snd $ filter (odd . fst) $ numbered xs

-- list-comprehension version
everyotherComp :: [a] -> [a]
everyotherComp xs = [ x | (i, x) <- zip [1..] xs, odd i ]


-- (c) same: positions where elements of two lists coincide
-- version using zip + zip with indices
same :: Eq a => [a] -> [a] -> [Int]
same xs ys = [ i | (i,(x,y)) <- zip [1..] (zip xs ys), x == y ]

-- cleaner / comprehension with zip3
sameComp :: Eq a => [a] -> [a] -> [Int]
sameComp xs ys = [ i | (i,x,y) <- zip3 [1..] xs ys, x == y ]
