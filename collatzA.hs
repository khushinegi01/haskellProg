-- The Collatz conjecture
-- A nice open problem in number theory is the Collatz conjecture, which supposes that for any number n greater than 1 , the sequence of the following two steps eventually reaches one:
-- n → 1/2n  if n is even
-- n → 3n+1 if n is odd
-- We can build this as a function in Haskell, but we don’t know if it will terminate!


-- Exercise 4:
-- a) Implement the function collatz :: Int -> Int to test the Collatz conjecture. It should return 1 when it terminates.

--  *Main> collatz 97
--  1
--  *Main> collatzCount 97 0
--  118
--  *Main> collatzMax 100 (0,0)
--  (97,118)

-- a)
collatz :: Int -> Int
collatz n
  | n <= 0    = error "collatz: input must be positive"
  | n == 1    = 1
  | even n    = collatz (n `div` 2)
  | otherwise = collatz (3 * n + 1)

main :: IO ()
main = print (collatz (negate 1))



-- 97 is odd →
-- → 3*97 + 1 = 292
-- → collatz 292

-- 292 even →
-- → 146

-- 146 even →
-- → 73

-- 73 odd →
-- → 3*73 + 1 = 220

-- 220 → 110 → 55
-- 55 → 166 → 83 → 250 → 125 → 376 → 188 → 94 → 47 → 142 → 71 → 214 → 107 → 322 → 161 → 484 → 242 → 121 → 364 → 182 → 91 → 274 → 137 → 412 → 206 → 103 → 310 → 155 → 466 → 233 → 700 → 350 → 175 → 526 → 263 → 790 → 395 → 1186 → 593 → 1780 → 890 → 445 → 1336 → 668 → 334 → 167 → 502 → 251 → 754 → 377 → 1132 → 566 → 283 → 850 → 425 → 1276 → 638 → 319 → 958 → 479 → 1438 → 719 → 2158 → 1079 → 3238 → 1619 → 4858 → 2429 → 7288 → 3644 → 1822 → 911 → 2734 → 1367 → 4102 → 2051 → 6154 → 3077 → 9232 → 4616 → 2308 → 1154 → 577 → 1732 → 866 → 433 → 1300 → 650 → 325 → 976 → 488 → 244 → 122 → 61 → 184 → 92 → 46 → 23 → 70 → 35 → 106 → 53 → 160 → 80 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1.

-- Yup. You finally hit 1.





