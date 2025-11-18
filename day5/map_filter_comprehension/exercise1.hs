-- helpers
double :: Num a => a -> a
double x = 2 * x

divisibleByThree :: Integral a => a -> Bool
divisibleByThree x = x `mod` 3 == 0

-- a) doubles: multiply every number by two

-- recursion
doublesRec :: Num a => [a] -> [a]
doublesRec []     = []
doublesRec (x:xs) = double x : doublesRec xs

-- map/filter
doublesMap :: Num a => [a] -> [a]
doublesMap = map double

-- list comprehension
doublesComp :: Num a => [a] -> [a]
doublesComp xs = [ double x | x <- xs ]


-- b) multiplesOfThree: keep only numbers divisible by 3

-- recursion
multiplesOfThreeRec :: Integral a => [a] -> [a]
multiplesOfThreeRec [] = []
multiplesOfThreeRec (x:xs)
  | divisibleByThree x = x : multiplesOfThreeRec xs
  | otherwise          = multiplesOfThreeRec xs

-- map/filter
multiplesOfThreeMap :: Integral a => [a] -> [a]
multiplesOfThreeMap = filter divisibleByThree

-- list comprehension
multiplesOfThreeComp :: Integral a => [a] -> [a]
multiplesOfThreeComp xs = [ x | x <- xs, divisibleByThree x ]


-- c) doubleMultiplesOfThree: take multiples of three and double them

-- recursion
doubleMultiplesOfThreeRec :: Integral a => [a] -> [a]
doubleMultiplesOfThreeRec [] = []
doubleMultiplesOfThreeRec (x:xs)
  | divisibleByThree x = double x : doubleMultiplesOfThreeRec xs
  | otherwise          = doubleMultiplesOfThreeRec xs

-- map/filter (composition)
doubleMultiplesOfThreeMap :: Integral a => [a] -> [a]
doubleMultiplesOfThreeMap = map double . filter divisibleByThree

-- list comprehension
doubleMultiplesOfThreeComp :: Integral a => [a] -> [a]
doubleMultiplesOfThreeComp xs = [ 2 * x | x <- xs, x `mod` 3 == 0 ]
