append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys


main = do 
    print(append [4,5,9] [0,1,7])



-- ### The code again:

-- ```haskell
-- append :: [a] -> [a] -> [a]
-- append [] ys = ys
-- append (x:xs) ys = x : append xs ys
-- ```

-- ### Step-by-step expansion

-- 1️⃣ First call:

-- ```
-- append [4,5,9] [0,1,7]
-- → matches (x:xs) = (4:[5,9])
-- → x = 4, xs = [5,9], ys = [0,1,7]
-- → so: 4 : append [5,9] [0,1,7]
-- ```

-- 2️⃣ Next call:

-- ```
-- append [5,9] [0,1,7]
-- → x = 5, xs = [9]
-- → 5 : append [9] [0,1,7]
-- ```

-- 3️⃣ Next call:

-- ```
-- append [9] [0,1,7]
-- → x = 9, xs = []
-- → 9 : append [] [0,1,7]
-- ```

-- 4️⃣ Base case hits:

-- ```
-- append [] [0,1,7] = [0,1,7]
-- ```


-- ### Now we stack back up (recursion unwinds)

-- Plugging results backward:

-- ```
-- append [9] [0,1,7]
-- = 9 : [0,1,7]
-- = [9,0,1,7]
-- ```

-- ```
-- append [5,9] [0,1,7]
-- = 5 : [9,0,1,7]
-- = [5,9,0,1,7]
-- ```

-- ```
-- append [4,5,9] [0,1,7]
-- = 4 : [5,9,0,1,7]
-- = [4,5,9,0,1,7]
-- ```

-- ✅ **Final result:** `[4,5,9,0,1,7]`
