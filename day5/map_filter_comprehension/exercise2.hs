-- Exercise 2 

-- part (a) 
doubles :: Num a => [a] -> [a]
doubles xs = map double xs
  where
    double y = 2 * y

multiplesOfThree :: Integral a => [a] -> [a]
multiplesOfThree xs = filter divisibleByThree xs
  where
    divisibleByThree n = n `mod` 3 == 0

doubleMultiplesOfThree :: Integral a => [a] -> [a]
doubleMultiplesOfThree xs = map double (filter divisibleByThree xs)
  where
    double y = 2 * y
    divisibleByThree n = n `mod` 3 == 0


-- part (b) 
-- using filter with a where-clause
shorts :: [String] -> [String]
shorts xs = filter isShort xs
  where
    isShort s = length s < 4

-- using list comprehension
shortsComp :: [String] -> [String]
shortsComp xs = [ s | s <- xs, length s < 4 ]


-- part (c) incrementPositives
-- using map and filter with where-clause
incrementPositives :: (Num a, Ord a) => [a] -> [a]
incrementPositives xs = map addOne (filter isPositive xs)
  where
    isPositive n = n > 0
    addOne n     = n + 1

-- using list comprehension
incrementPositivesComp :: (Num a, Ord a) => [a] -> [a]
incrementPositivesComp xs = [ n + 1 | n <- xs, n > 0 ]


-- part (d) difference
-- using filter and elem (general for any Eq)
difference :: Eq a => [a] -> [a] -> [a]
difference s1 s2 = filter (`notElem` s2) s1
  where
    -- helper not necessary but shown per instruction style:
    notElem x ys = not (x `elem` ys)

-- using list comprehension
differenceComp :: Eq a => [a] -> [a] -> [a]
differenceComp s1 s2 = [ c | c <- s1, c `notElem` s2 ]


-- part (e) oddLengthSums
-- using map and filter with where-clause
oddLengthSums :: Num a => [[a]] -> [a]
oddLengthSums xss = map sum (filter hasOddLength xss)
  where
    hasOddLength xs = odd (length xs)

-- using list comprehension
oddLengthSumsComp :: Num a => [[a]] -> [a]
oddLengthSumsComp xss = [ sum xs | xs <- xss, odd (length xs) ]


