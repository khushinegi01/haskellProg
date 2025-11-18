combinations :: [a] -> [b] -> [(a,b)]
combinations xs ys = [ (x,y) | x <- xs, y <- ys ]


selfcombinations :: [a] -> [(a,a)]
selfcombinations xs =
    [ (x,y)
    | (i,x) <- zip [0..] xs
    , y     <- drop i xs
    ]


pyts :: Int -> [(Int, Int, Int)]
pyts n =
    [ (a,b,c)
    | a <- [1..n]
    , b <- [1..n]
    , c <- [1..n]
    , a*a + b*b == c*c
    ]
