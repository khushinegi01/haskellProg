-- c) Implement the function collatzMax::Int -> (Int,Int) -> (Int,Int) so that collatzMax n (0,0) finds the number between 1 and n that requires the most steps to reach 1. Use the second argument in collatzMax n (m,s) to remember the current number m with the longest sequence of s steps.


collatzCount :: Int -> Int -> Int
collatzCount n count
  | n <= 0    = error "collatzCount: input must be positive"
  | n == 1    = count
  | even n    = collatzCount (n `div` 2) (count + 1)
  | otherwise = collatzCount (3 * n + 1) (count + 1)



collatzMax :: Int -> (Int,Int) -> (Int,Int)
collatzMax n (bestNum, bestSteps)
  | n <= 0    = error "collatzMax: n must be positive"
  | n == 1    =
      let steps1 = collatzCount 1 0
      in if steps1 > bestSteps then (1, steps1) else (bestNum, bestSteps)
  | otherwise =
      let stepsN = collatzCount n 0
          (newBestNum, newBestSteps) =
            if stepsN > bestSteps then (n, stepsN) else (bestNum, bestSteps)
      in collatzMax (n - 1) (newBestNum, newBestSteps)



main = do
    print(collatzMax 100 (0,0))




-
-- We start from:

-- ```haskell
-- collatzMax 100 (0,0)
-- ```

-- Here `(0,0)` means:

-- * bestNum = 0 (we haven’t found anything yet)
-- * bestSteps = 0 (no steps yet)


-- ### 🧩 Step 1 — Evaluate `collatzMax 100 (0,0)`

-- The function goes:

-- ```haskell
-- stepsN = collatzCount 100 0
-- ```

-- Now it runs `collatzCount` for 100.


-- ### Step 2 — `collatzCount 100 0`

-- The rule:

-- * if even → divide by 2
-- * if odd → 3n + 1
-- * count each move

-- So:

-- ```
-- 100 → 50 → 25 → 76 → 38 → 19 → 58 → 29 → 88 → 44 → 22 → 11 
-- → 34 → 17 → 52 → 26 → 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- ```

-- That’s **25 steps** total.

-- So, `collatzCount 100 0` returns `25`.

-- Now back to `collatzMax`:

-- ```haskell
-- stepsN = 25
-- (bestNum, bestSteps) = (0,0)
-- newBest = (100, 25)  -- because 25 > 0
-- ```

-- Now it calls recursively:

-- ```haskell
-- collatzMax 99 (100,25)
-- ```

-- ---

-- ### 🧩 Step 3 — Evaluate `collatzMax 99 (100,25)`

-- Same process:
-- `stepsN = collatzCount 99 0`

-- 99 → 298 → 149 → 448 → … → 1
-- That takes **25 steps** too.
-- Not greater than bestSteps (25), so it keeps `(100,25)`.

-- Next:

-- ```haskell
-- collatzMax 98 (100,25)
-- ```

-- ---

-- ### 🧩 Step 4 — Eventually…

-- This continues downward:

-- * For each number from 98 down to 1,
-- * It computes its Collatz length
-- * Compares with the current “best”
-- * If it’s longer, updates the best pair.

-- ---

-- ### 💪 The interesting moment

-- When it hits `n = 97`:

-- ```haskell
-- collatzCount 97 0
-- ```

-- That gives **118 steps** — which beats the previous best (25).

-- So we now store:

-- ```
-- (bestNum, bestSteps) = (97,118)
-- ```

-- Then it keeps checking 96 → 95 → … → 1,
-- but none exceed 118 steps.

-- ---

-- ### 🏁 Final step — when `n == 1`

-- The base case:

-- ```haskell
-- collatzMax 1 (bestNum, bestSteps)
-- ```

-- Since 1 has 0 steps, it doesn’t beat 118,
-- so it returns the final tuple:

-- ```
-- (97,118)
-- ```

-- ---

-- ### 🖥️ So, what `print (collatzMax 100 (0,0))` actually does:

-- 1. Iterates from 100 down to 1
-- 2. Calculates Collatz sequence length for each number
-- 3. Keeps track of the one with the max steps
-- 4. Returns that number and its step count

-- ### ✅ Output

-- ```
-- (97,118)
-- ```

-- ---

-- ### 🧠 TL;DR:

-- * The `collatzCount` counts how long each number’s chain takes.
-- * The `collatzMax` hunts for the one with the longest chain.
-- * For 1–100, the winner is **97**, taking **118 steps** to hit 1.

-- If you printed intermediate results, you’d literally watch it climb from `(100,25)` → `(97,118)` and stop there.
