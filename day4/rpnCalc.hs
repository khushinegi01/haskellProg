-- Main.hs
-- RPN Calculator with safe error handling and interactive input

import Text.Read (readMaybe)
import Control.Monad (foldM)

-- =====================
-- RPN Evaluator
-- =====================

runRPN :: String -> Either String Double
runRPN input = do
  let tokens = words input
  finalStack <- foldM step [] tokens
  case finalStack of
    [result] -> Right result
    []       -> Left "Empty expression."
    _        -> Left $ "Leftover values on stack: " ++ show finalStack

step :: [Double] -> String -> Either String [Double]
step stack tok =
  case tok of
    "+" -> binaryOp stack (+)
    "-" -> binaryOp stack (-)
    "*" -> binaryOp stack (*)
    "/" -> safeDiv stack
    "^" -> binaryOp stack (**)
    _   -> case readMaybe tok :: Maybe Double of
             Just n  -> Right (n : stack)
             Nothing -> Left $ "Unknown token: " ++ show tok

binaryOp :: [Double] -> (Double -> Double -> Double) -> Either String [Double]
binaryOp (x:y:rest) op = Right (op y x : rest)
binaryOp _ _           = Left "Not enough operands for binary operator."

safeDiv :: [Double] -> Either String [Double]
safeDiv (x:y:rest)
  | x == 0    = Left "Division by zero."
  | otherwise = Right (y / x : rest)
safeDiv _ = Left "Not enough operands for division."

-- =====================
-- MAIN: interact or test directly
-- =====================

main :: IO ()
main = do
  putStrLn "==============================="
  putStrLn "   Reverse Polish Calculator   "
  putStrLn "==============================="
  putStrLn "Enter an RPN expression (example: 1 2 + 3 4 + *):"
  putStr "> "
  expr <- getLine
  case runRPN expr of
    Right result -> putStrLn $ " Result: " ++ show result
    Left err     -> putStrLn $ " Error: " ++ err
