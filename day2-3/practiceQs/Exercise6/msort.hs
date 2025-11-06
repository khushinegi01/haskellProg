msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  =
  let (left, right) = splitAt (length xs `div` 2) xs
  in merge (msort left) (msort right)


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys   -- equal elements: keep one



main = do 
    print(msort [5,7,1,4,9,2,8,6,3])


-- Step-by-step evaluation example

-- Let’s trace msort [3,1,2]:

-- length [3,1,2] = 3, half = 1
-- → split into [3] and [1,2]

-- msort [3] = [3]

-- msort [1,2]
-- → split [1] and [2]
-- → merge [1] [2] = [1,2]

-- Merge [3] and [1,2]:

-- compare 3 and 1 → keep 1

-- compare 3 and 2 → keep 2

-- now second list empty → keep [3]

--  Result: [1,2,3]