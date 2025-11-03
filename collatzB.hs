-- b) Adapt the Collatz function as collatzCount::Int -> Int -> Int so that it counts the number of steps taken. Use the extra input to count steps, and change the output to return the number of steps instead of always 1 .


collatzCount :: Int -> Int -> Int
collatzCount n count
  | n <= 0    = error "collatzCount: input must be positive"
  | n == 1    = count
  | even n    = collatzCount (n `div` 2) (count + 1)
  | otherwise = collatzCount (3 * n + 1) (count + 1)


main = do
    print (collatzCount 97 0)
