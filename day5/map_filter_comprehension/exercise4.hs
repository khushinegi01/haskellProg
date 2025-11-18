-- Exercise 1 (simplified)
doubles :: Num a => [a] -> [a]
doubles = map (* 2)                       -- partial application / section

multiplesOfThree :: Integral a => [a] -> [a]
multiplesOfThree = filter ((== 0) . (`mod` 3))  -- composition + section

doubleMultiplesOfThree :: Integral a => [a] -> [a]
doubleMultiplesOfThree = map (*2) . filter ((==0) . (`mod` 3))  -- compose map after filter


-- Exercise 2 (simplified)
shorts :: [String] -> [String]
shorts = filter ((< 4) . length)          -- section + composition

shortsComp :: [String] -> [String]
shortsComp xs = [ s | s <- xs, length s < 4 ]  -- kept comprehension (already concise)

incrementPositives :: (Num a, Ord a) => [a] -> [a]
incrementPositives = map (+1) . filter (> 0)   -- map after filter, (+1) is a section

incrementPositivesComp :: (Num a, Ord a) => [a] -> [a]
incrementPositivesComp xs = [ n + 1 | n <- xs, n > 0 ]

difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = filter (`notElem` ys) xs   -- nice and short; could eta-reduce but readable

differenceComp :: Eq a => [a] -> [a] -> [a]
differenceComp s1 s2 = [ c | c <- s1, c `notElem` s2 ]

oddLengthSums :: Num a => [[a]] -> [a]
oddLengthSums = map sum . filter (odd . length)  -- composition of map and filter

oddLengthSumsComp :: Num a => [[a]] -> [a]
oddLengthSumsComp xss = [ sum xs | xs <- xss, odd (length xs) ]


-- Exercise 3 (simplified)
numbered :: [a] -> [(Int,a)]
numbered = zip [1..]                         -- classic point-free

everyother :: [a] -> [a]
everyother = map snd . filter (odd . fst) . zip [1..]  -- pipe: zip -> filter -> map

everyotherComp :: [a] -> [a]
everyotherComp xs = [ x | (i,x) <- zip [1..] xs, odd i ]

same :: Eq a => [a] -> [a] -> [Int]
same xs ys = map fst $ filter (\(_, (x,y)) -> x == y) $ zip [1..] $ zip xs ys
-- or slightly point-free but less readable:
-- same = map fst . filter (\(_, (x,y)) -> x == y) .: zip [1..] . zip
-- where (.:) = (.) . (.)    -- I left that out because it's obscure for learners

sameComp :: Eq a => [a] -> [a] -> [Int]
sameComp xs ys = [ i | (i,x,y) <- zip3 [1..] xs ys, x == y ]
