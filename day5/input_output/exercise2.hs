-- Lizzy.hs
-- Small interactive "psychologist" demo

module Lizzy where

import System.Random (randomIO)

-- fixed strings
welcome, exitMsg :: String
welcome = "Dr. Lizzy -- Good morning, how are you today. Please tell me\nwhat's on your mind."
exitMsg = "Dr. Lizzy -- Goodbye. Take care."

-- a table of 3 * 5 = 15 possible replies (3 rows × 5 columns)
responses :: [[String]]
responses =
  [ [ "Let's examine that more closely, shall we."
    , "Please tell me more about that."
    , "That sounds important — go on."
    , "Hmm... say a bit more."
    , "Why do you think that is?"
    ]
  , [ "Do you think this has something to do with your mother?"
    , "How does that make you feel right now?"
    , "When did this start?"
    , "Do other people notice this too?"
    , "Have you felt like this before?"
    ]
  , [ "I'm curious — what do you want to change about this?"
    , "That's understandable; what would help?"
    , "Are there small steps you can take?"
    , "Who do you talk to about this?"
    , "It might help to explore that further."
    ]
  ]

-- deterministic response chosen from responses table using length of input
response :: String -> String
response str =
  let idx = length str `mod` 15         -- 0..14
      row = idx `div` 5                -- 0..2
      col = idx `mod` 5                -- 0..4
  in responses !! row !! col

-- randomresponse picks an entry from the same table but using an integer r
randomresponse :: Int -> String -> String
randomresponse r _str =
  let i   = abs r `mod` 15
      row = i `div` 5
      col = i `mod` 5
  in responses !! row !! col

-- part (a): show welcome message
lizzy :: IO ()
lizzy = do
  putStrLn welcome
  lizzyLoop            -- part (d): begin the interactive loop

-- part (b)/(c): single-step loop that reads a line and responds randomly
lizzyLoop :: IO ()
lizzyLoop = do
  str <- getLine                     -- read user input
  r   <- randomIO :: IO Int          -- draw a random Int
  putStrLn (randomresponse r str)    -- print a random response
  lizzyLoop                          -- loop (call itself again)
