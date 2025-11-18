-- Exercise 2 

module Extra2 where

import Data.List (nub)

-- a + b) Suit with derived Eq, Ord, Show
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Show)

-- c) red and black
red :: Suit -> Bool
red Diamonds = True 
red Hearts   = True
red _        = False

black :: Suit -> Bool
black = not . red

-- d) Face type and derive Enum/Ord/Show/Eq
data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show, Enum, Bounded)

-- e) next face (using Enum)
next :: Face -> Maybe Face
next Ace = Nothing
next f   = Just (succ f)

-- f) Card type
type Card = (Suit, Face)

-- helper: consecutive faces
consecutive :: [Face] -> Bool
consecutive xs = length xs >= 3 && and (zipWith (\a b -> succ a == b) xs (tail xs))

-- helper: all suits distinct
allDistinctSuits :: [Card] -> Bool
allDistinctSuits cs = length (nub suits) == length suits
  where suits = map fst cs

-- g) series: at least 3 cards, either same suit & consecutive faces,
-- or different suits & same face
series :: [Card] -> Bool
series cs
  | length cs < 3 = False
  | sameSuit && consecutive faces = True
  | sameFace && allDistinctSuits cs = True
  | otherwise = False
  where
    suits = map fst cs
    faces = map snd cs
    sameSuit = all (== head suits) suits
    sameFace = all (== head faces) faces
