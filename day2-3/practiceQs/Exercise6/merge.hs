merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys   -- equal elements: keep one


main = do 
    print(merge [1,2,4,5,6,9] [2,3,6,7,8])



-- merge [1,2,4] [2,3]
-- Evaluation steps:

-- Compare heads: 1 < 2 → keep 1
-- → 1 : merge [2,4] [2,3]

-- Now: 2 == 2 → keep one 2
-- → 1 : 2 : merge [4] [3]

-- Compare heads: 4 > 3 → keep 3
-- → 1 : 2 : 3 : merge [4] []

-- Second list empty → return [4]

-- ✅ Result: [1,2,3,4]