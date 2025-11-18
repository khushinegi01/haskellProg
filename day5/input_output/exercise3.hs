-- TicTacToe.hs
-- noughts-and-crosses with Human / Random AI / Smart AI (minimax)
-- GHC 8+ / GHCi

module Main where

import System.Random (randomRIO)
import Data.Maybe (isNothing, fromMaybe)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.Ord (comparing)

-- Board: list of 9 Maybe Char (Nothing = empty, Just 'X' or Just 'O')
type Board = [Maybe Char]

emptyBoard :: Board
emptyBoard = replicate 9 Nothing

-- Players
data PlayerKind = Human | RandomComp | SmartComp deriving (Eq, Show)

-- Helper: other player char
other :: Char -> Char
other 'X' = 'O'
other 'O' = 'X'
other  c  = c

-- Pretty printing: show index mapping and board
prettyBoard :: [Maybe Char] -> String
prettyBoard b =
  let showCell :: Maybe Char -> String
      showCell Nothing  = "   "
      showCell (Just c) = " " ++ [c] ++ " "

      row i = intercalate "|" $ map showCell (take 3 (drop (i*3) b))
      sep = "\n---+---+---\n"
  in " " ++ row 0 ++ sep ++ " " ++ row 1 ++ sep ++ " " ++ row 2 ++ "\n"

showBoard :: Board -> String
showBoard b = prettyBoard (map (fmap id) b)

-- Utility: print board with nicer spaces
printBoard :: Board -> IO ()
printBoard b = putStrLn $ showBoard (map (fmap id) b)

-- Available moves: indices 0..8 that are Nothing
availableMoves :: Board -> [Int]
availableMoves b = [ i | (i,cell) <- zip [0..] b, isNothing cell ]

-- Make move: index 0..8, char 'X' or 'O'
makeMove :: Board -> Int -> Char -> Board
makeMove b i c = take i b ++ [Just c] ++ drop (i+1) b

-- Win lines
winningLines :: [[Int]]
winningLines =
  [ [0,1,2], [3,4,5], [6,7,8]           -- rows
  , [0,3,6], [1,4,7], [2,5,8]           -- cols
  , [0,4,8], [2,4,6]                    -- diags
  ]

-- Check winner: returns Just 'X' or Just 'O' or Nothing
winner :: Board -> Maybe Char
winner b = foldr checkLine Nothing winningLines
  where
    checkLine _ (Just c) = Just c
    checkLine line Nothing =
      case map (b !!) line of
        [Just a, Just b', Just c] | a == b' && b' == c -> Just a
        _ -> Nothing

isFull :: Board -> Bool
isFull = null . availableMoves

-- Prompt helpers
promptLine :: String -> IO String
promptLine msg = putStr msg >> getLine

-- Choose player kind
choosePlayer :: Char -> IO PlayerKind
choosePlayer who = do
  putStrLn $ "Choose player for " ++ [who] ++ " — (h)uman, (r)andom computer, (s)mart computer:"
  line <- getLine
  case line of
    ('h':_) -> return Human
    ('H':_) -> return Human
    ('r':_) -> return RandomComp
    ('R':_) -> return RandomComp
    ('s':_) -> return SmartComp
    ('S':_) -> return SmartComp
    _       -> putStrLn "Please type h, r, or s." >> choosePlayer who

-- Random move chooser
getRandomMove :: Board -> IO Int
getRandomMove b = do
  let moves = availableMoves b
  i <- randomRIO (0, length moves - 1)
  return (moves !! i)

-- Smart move chooser using minimax
-- Score: +1 for X win, -1 for O win, 0 for draw
minimaxScore :: Board -> Char -> Int
minimaxScore b current =
  case winner b of
    Just 'X' -> 1
    Just 'O' -> -1
    Nothing  ->
      if isFull b then 0
      else
        let moves = availableMoves b
            next = other current
            scores = [ minimaxScore (makeMove b m current) next | m <- moves ]
        in if current == 'X' then maximum scores else minimum scores

-- Get best moves (pure) then pick random among them in IO
getSmartMove :: Board -> Char -> IO Int
getSmartMove b current = do
  let moves = availableMoves b
      scored = [ (m, minimaxScore (makeMove b m current) (other current)) | m <- moves ]
      bestScore = if current == 'X' then maximum (map snd scored) else minimum (map snd scored)
      bestMoves = [ m | (m,s) <- scored, s == bestScore ]
  idx <- randomRIO (0, length bestMoves - 1)
  return (bestMoves !! idx)

-- Single-turn: get move for given player kind and char
getMoveFor :: PlayerKind -> Board -> Char -> IO Int
getMoveFor Human b _ = do
  putStrLn $ "Choose your move from: " ++ unwords (map show $ map (+1) $ availableMoves b) ++ ": "
  line <- getLine
  case reads line :: [(Int,String)] of
    [(n, _)] | n >= 1 && n <= 9 && (isNothing (b !! (n-1))) -> return (n-1)
    _ -> putStrLn "Invalid move — try again." >> getMoveFor Human b 'X'
getMoveFor RandomComp b _ = getRandomMove b
getMoveFor SmartComp b c = getSmartMove b c

-- Game loop, returns winner Maybe Char or Nothing for draw
gameLoop :: Board -> (PlayerKind, PlayerKind) -> Char -> IO ()
gameLoop b (kx, ko) turn = do
  putStrLn "\nCurrent board:"
  printBoard b
  case winner b of
    Just c -> putStrLn $ "Game over! Winner: " ++ [c]
    Nothing ->
      if isFull b then putStrLn "Game over! It's a draw."
      else do
        let kind = if turn == 'X' then kx else ko
        move <- getMoveFor kind b turn
        let b' = makeMove b move turn
        -- After move, check winner/draw
        case winner b' of
          Just c -> do
            putStrLn "\nFinal board:"
            printBoard b'
            putStrLn $ "Game over! Winner: " ++ [c]
          Nothing ->
            if isFull b' then do
              putStrLn "\nFinal board:"
              printBoard b'
              putStrLn "Game over! It's a draw."
            else gameLoop b' (kx, ko) (other turn)

-- Intro mapping helper
showMapping :: IO ()
showMapping = do
  putStrLn "Welcome to noughts and crosses"
  putStrLn "Choose your moves from the numbered board below (positions 1..9):"
  putStrLn $ prettyBoard (map Just "123456789")  -- prints mapping 1..9

-- Main: choose players and start loop
main :: IO ()
main = do
  showMapping
  kx <- choosePlayer 'X'
  ko <- choosePlayer 'O'
  putStrLn $ "Starting: X = " ++ show kx ++ ", O = " ++ show ko
  gameLoop emptyBoard (kx, ko) 'X'
