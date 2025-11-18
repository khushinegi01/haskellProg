-- Exercise 1 

module Extra1 where

-- a) constructor types (informal)
-- False :: Bool
-- True  :: Bool
-- Nothing :: Maybe a
-- Just   :: a -> Maybe a
-- Left   :: a -> Either a b
-- Right  :: b -> Either a b

-- b) safe integer division
divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (x `div` y)

-- c) plus on Maybe Int
plus :: Maybe Int -> Maybe Int -> Maybe Int
plus (Just a) (Just b) = Just (a + b)
plus _        _        = Nothing

-- d) lookup (find)
find :: Eq a => a -> [(a,b)] -> Maybe b
find _ [] = Nothing
find k ((x,y):xs)
  | k == x    = Just y
  | otherwise = find k xs

-- e) removeMaybe: drop Nothing, extract Just x
removeMaybe :: [Maybe a] -> [a]
removeMaybe [] = []
removeMaybe (Nothing:xs) = removeMaybe xs
removeMaybe (Just x:xs)  = x : removeMaybe xs

-- f) toEither and toPair
toEither :: (Bool, a) -> Either a a
toEither (True, a)  = Right a
toEither (False, a) = Left a

toPair :: Either a a -> (Bool, a)
toPair (Left a)  = (False, a)
toPair (Right a) = (True, a)

-- g) splitEither
splitEither :: [Either a b] -> ([a], [b])
splitEither = go ([],[])
  where
    go (as,bs) []               = (reverse as, reverse bs)
    go (as,bs) (Left a : xs)   = go (a:as, bs) xs
    go (as,bs) (Right b : xs)  = go (as, b:bs) xs
