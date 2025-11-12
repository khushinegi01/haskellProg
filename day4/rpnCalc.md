
## Overview

This Haskell program **evaluates expressions written in Reverse Polish Notation (RPN)** — also known as **postfix notation** (like `3 4 +` instead of `3 + 4`).
It returns either:

* a **result** (as `Right result`), or
* an **error message** (as `Left "message"`).


##  Imports

```haskell
import Text.Read (readMaybe)
import Control.Monad (foldM)
```

* `readMaybe` safely tries to turn a string like `"3.5"` into a number (`Maybe Double`), without crashing on invalid input.
* `foldM` is a version of `fold` that works with monads like `Either`. It lets us “loop” over tokens while carrying possible errors.


##  Main function: `runRPN`

```haskell
runRPN :: String -> Either String Double
runRPN input = do
  let tokens = words input
  finalStack <- foldM step [] tokens
  case finalStack of
    [result] -> Right result
    []       -> Left "empty expression"
    _        -> Left $ "leftover values on stack: " ++ show finalStack
```

### Step by step

1. `runRPN :: String -> Either String Double`

   * Takes a string (like `"3 4 +"`).
   * Returns either `Left "error"` or `Right result`.

2. `let tokens = words input`

   * Splits the string into separate tokens by spaces.
     Example: `"3 4 +"` → `["3", "4", "+"]`.

3. `finalStack <- foldM step [] tokens`

   * Starts with an **empty stack** (`[]`).
   * For each token, applies `step`, which updates the stack.
   * `foldM` automatically stops and returns `Left error` if `step` fails at any point.

4. After processing all tokens, we check the stack:

   * If it has **exactly one number**, that’s the final answer → `Right result`.
   * If it’s **empty**, that means no expression → `Left "empty expression"`.
   * If there are **extra values left**, the input was malformed → error message with the leftover stack.


##  The helper function `step`

```haskell
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
             Nothing -> Left $ "unknown token: " ++ show tok
```

### What it does

* It looks at one token (`tok`) and updates the stack.

### How it works

* If the token is `"+"`, `"-"`, `"*"`, `"/"`, or `"^"`, it performs the corresponding arithmetic operation using helper functions (`binaryOp` or `safeDiv`).
* If the token is not an operator:

  * Try to read it as a number (`readMaybe tok :: Maybe Double`).
  * If it succeeds (`Just n`), push it onto the stack (`n : stack`).
  * If it fails (`Nothing`), it’s an invalid token → return an error message.


##  The helper `binaryOp`

```haskell
binaryOp :: [Double] -> (Double -> Double -> Double) -> Either String [Double]
binaryOp (x:y:rest) op = Right (op y x : rest)
binaryOp _ _           = Left "stack underflow: not enough operands for binary operator"
```

### What it does

Performs an operation (`+`, `-`, `*`, `^`, etc.) on the **top two numbers of the stack**.

### Explanation

* Pattern `(x:y:rest)` means: top two numbers are `x` and `y`, rest of stack is `rest`.
* Compute `op y x` (note the order — the top of stack `x` is the *second operand*).
* Push the result back on the stack: `op y x : rest`.
* If there aren’t at least two numbers, it returns an error: **stack underflow**.

Example:

```
Stack before: [4, 3]
Operation: (+)
Result pushed: 3 + 4 = 7
Stack after: [7]
```


##  Safe division `safeDiv`

```haskell
safeDiv :: [Double] -> Either String [Double]
safeDiv (x:y:rest)
  | x == 0    = Left "division by zero"
  | otherwise = Right (y / x : rest)
safeDiv _ = Left "stack underflow: not enough operands for /"
```

### What it does

Handles division, but checks for divide-by-zero.

### Explanation

* If stack has at least two numbers (`x` and `y`):

  * If `x == 0`, error “division by zero”.
  * Otherwise push result of `y / x`.
* If fewer than two numbers, error “stack underflow”.


##  Example

For input:

```
"3 4 + 2 *"
```

Steps:

1. push 3 → `[3]`
2. push 4 → `[4,3]`
3. `+` → pop 4 & 3, push 7 → `[7]`
4. push 2 → `[2,7]`
5. `*` → pop 2 & 7, push 14 → `[14]`
   Result: `Right 14.0`


 **Summary:**

* `runRPN` reads an expression, splits into tokens, and processes each.
* `step` decides what to do with each token.
* `binaryOp` and `safeDiv` do arithmetic safely.
* Errors are handled gracefully using `Either`.

You can run `runRPN "3 4 + 2 *"` and get `Right 14.0`.
