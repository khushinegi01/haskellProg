-- The Ackermann–Péter function

-- The Ackermann–Péter function (often just called the Ackermann function) was designed as an example of a computable function that is not primitive recursive. It grows very quickly!
-- A(0,n) = n+1 
-- A(m+1,0) = A(m,1)
-- A(m+1,n+1) = A(m,A(m+1,n))


-- a) Implement the Ackermann function as ackermann :: Int -> Int -> Int . Try it out for very small values.
--  *Main> ackermann 2 2
--  7
--  *Main> ackermann 3 7
--  1021

ackermann :: Int -> Int -> Int
ackermann m n
  | m < 0 || n < 0 = error "ackermann: negative input"
  | m == 0         = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | otherwise      = ackermann (m - 1) (ackermann m (n - 1))

-- A(m,n)
-- A(2,2)
-- → A(1, A(2,1))
-- → A(1, A(1, A(2,0)))
-- → A(1, A(1, A(1,1)))
-- → A(1, A(1, A(0, A(1,0))))
-- → A(1, A(1, A(0, A(0,1))))
-- → A(1, A(1, A(0,2)))
-- → A(1, A(1,3))
-- → A(1, A(0, A(1,2)))
-- → A(1, A(0, A(0, A(1,1))))
-- → A(1, A(0, A(0, A(0, A(1,0)))))
-- → A(1, A(0, A(0, A(0, A(0,1))))) = 7


main = do
    print(ackermann 2 2)
    print (ackermann 3 7)