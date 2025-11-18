repeatMe :: IO ()
repeatMe = do
    x <- getLine
    putStr "You just told me: "
    putStrLn x
