# FUNCTIONS & FUNCTION TYPES (Topics 5–10) — Deep, beginner-friendly explanation with examples & practice

Nice — you’ve got the core of Haskell here. I’ll walk you through each topic step-by-step, show how things evaluate (so you *see* what’s happening), give concrete examples, and finish with practice questions and answers/hints so you can actually *do* it. I’ll keep the tone chilled but slightly sarcastic, because learning functional programming without some attitude is just wrong.

---

# 1. Everything is a function (currying & function types)

### Idea (short)

In Haskell every function takes exactly one argument and returns exactly one result. When you see something like `Int -> Int -> Int`, it’s actually `Int -> (Int -> Int)`: a function that returns another function. This is **currying**.

### Example

```hs
addOne :: Int -> Int
addOne x = x + 1

add :: Int -> Int -> Int
add x y = x + y
```

`add` *looks* like it takes two arguments, but under the hood:

* `add 5` is a function of type `Int -> Int`
* `(add 5) 3` evaluates to `8`

Step-by-step evaluation:

1. `add 5 3` → `((add 5) 3)`
2. `add 5` yields a function `\y -> 5 + y`
3. Applying that to `3` gives `5 + 3 = 8`

### Why it’s useful

* You can create specialized functions easily: `add5 = add 5` — now `add5 10` → 15.
* Functions are easy to compose and pass around.


---

# 2. Referential transparency (purity)

### Idea (short)

An expression is **referentially transparent** if you can replace it with its value anywhere and nothing changes. Haskell is mostly pure: functions don’t have hidden side effects (unless you deliberately use IO or unsafe features).

### Example (pure)

```hs
square x = x * x
-- wherever you see `square 3`, you can replace it with `9`
```

### Example (impure, compare)

In Python or imperative languages, a function that depends on or changes external state is NOT referentially transparent:

```python
# pseudocode
count = 0
def inc():
    global count
    count += 1
    return count
```

Here `inc()` depends on and mutates `count`, so `inc()` cannot be substituted by a single value.

### Benefits

* Easier reasoning: no hidden surprises.
* Safer parallelism: computations can run in any order.
* Memoization/caching becomes possible.

### Practice

Is `randomRIO (1,10)` referentially transparent? (Answer: no — it returns different values each call.)

---

# 3. Pairs (tuples)

### Idea (short)

A pair is a fixed-length container for two values, possibly of different types. Syntax: `(a, b)`. Use pattern matching to get elements.

### Example

```hs
pair :: (Int, String)
pair = (42, "Answer")

first (x, _) = x
second (_, y) = y

-- usage:
-- first (10, "hi")  ==> 10
-- second (10, "hi") ==> "hi"
```

### Notes

* Tuples can have any finite size: `(a,b,c,...)`. But they’re fixed-size; lists are variable-length but homogeneous.
* If you want a named record, use `data` with fields (but that’s next level).

### Practice

Write a function `swap :: (a, b) -> (b, a)` that swaps a pair.

---

# 4. Lists — the backbone

### Idea (short)

Lists are ordered collections of elements of the *same type*. Syntax: `[1,2,3]`. Important operations: `head`, `tail`, `last`, `init`, and the cons operator `(:)` which is fast for adding to the front.

### Examples & evaluation

```hs
nums = [1,2,3,4,5]
head nums -- 1
tail nums -- [2,3,4,5]
last nums -- 5
init nums -- [1,2,3,4]

0 : nums -- [0,1,2,3,4,5]
```

### Recursion over lists

A canonical pattern:

```hs
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs
```

Evaluate `sumList [1,2,3]`:

* `1 + sumList [2,3]`
* `1 + (2 + sumList [3])`
* `1 + (2 + (3 + sumList []))`
* `1 + (2 + (3 + 0)) = 6`

Think: pattern match on `[]` and on `(x:xs)`. Haskellers think “head + tail”, not “index + loop”.

### Common list functions (built-in)

* `map`, `filter`, `foldr`, `foldl`, `++` (concat), `take`, `drop`, `length`, `reverse`.

### Practice

Implement `myLength :: [a] -> Int` using recursion (no built-ins).

---

# 5. Higher-order functions (HOFs)

### Idea (short)

A *higher-order function* takes functions as arguments and/or returns functions. This allows powerful abstractions.

### map

Custom `map'`:

```hs
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs
```

Example: `map' (*2) [1,2,3]` → `[2,4,6]`

### filter

Custom `filter'`:

```hs
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs
```

Example: `filter' even [1,2,3,4]` → `[2,4]`

### applyTwice example (function as argument and return)

```hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice (*2) 3      -- 12 (3 -> 6 -> 12)
applyTwice reverse "abc" -- "abc" (reverse "abc" = "cba", reverse "cba" = "abc")
```

### Currying + partial application synergy

Because everything is curried, `map (+1)` is a function waiting for a list. `map (+1) [1,2]` → `[2,3]`.

### Practice

1. Implement `compose` (function composition) in Haskell:

   ```hs
   compose :: (b -> c) -> (a -> b) -> a -> c
   compose f g x = f (g x)
   ```
2. Use `compose` to make a function that takes an `Int`, doubles it, then adds 3.

---

# 6. foldr (reduction)

### Idea (short)

`foldr` consumes a list and combines its elements using a function and a base value. It’s the right-associative fold.

Signature:

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
```

### Examples

* `foldr (+) 0 [1,2,3]` → `1 + (2 + (3 + 0))` → `6`
* `foldr (++) "" ["Ha","sk","ell"]` → `"Haskell"`

`foldr` can rebuild lists, maps, filters, etc.

### Implementing map with foldr

```hs
mapWithFold :: (a -> b) -> [a] -> [b]
mapWithFold f = foldr (\x acc -> f x : acc) []
```

### Why foldr, not foldl?

* `foldr` works well with infinite lists and lazy computations in many contexts.
* `foldl` (strict variant `foldl'`) is often used to avoid stack overflows for large lists because it is tail-recursive/strict.

### Practice

Use `foldr` to implement `filter`:

```hs
filterWithFold :: (a -> Bool) -> [a] -> [a]
filterWithFold p = foldr (\x acc -> if p x then x:acc else acc) []
```

---

# 7. Code quality — how to write nice Haskell

### Core principles

* **Explicit type signatures.** They’re docs + safety. Example: `processNumbers :: [Int] -> [Int]`.
* **Small, composable functions.** Each function should do one thing.
* **Avoid side effects.** Keep functions pure unless you must use IO.
* **Meaningful names.** `f x` is not great; `incrementSalary` is.
* **Use function composition** where it makes code clearer.

### Example

```hs
processNumbers :: [Int] -> [Int]
processNumbers = map (+1) . filter even
-- Equivalent:
-- processNumbers xs = map (+1) (filter even xs)
```

`(.)` composes `map (+1)` after `filter even`. It reads right-to-left: `filter` then `map`.

### Tip about naming & readability

Prefer clear pipe-like combinations:

```hs
result = (map (+1) . filter even) xs
```

or

```hs
let evens = filter even xs
in map (+1) evens
```

If composition gets too dense, break it into named steps.

---

# 8. Short examples & combined walkthroughs

### Currying + HOF + foldr example

Write a function that given a list of integers, keeps only odd numbers, squares them, and sums them up:

```hs
sumOfSquaresOfOdds :: [Int] -> Int
sumOfSquaresOfOdds = foldr (\x acc -> x * x + acc) 0 . filter odd
```

Step-by-step for `[1,2,3]`:

1. `filter odd [1,2,3]` → `[1,3]`
2. `foldr (\x acc -> x*x + acc) 0 [1,3]` → `1*1 + (3*3 + 0)` → `1 + 9 = 10`

(You could also write it with `map` + `sum`: `sum . map (^2) . filter odd`.)

---

# 9. Practice questions (with hints & answers)

### Beginner exercises

1. **Currying**

   * Q: Define `add :: Int -> Int -> Int`. Create `add7` and evaluate `add7 5`.
   * Hint: `add7 = add 7`.
   * Answer: `add7 5` → `12`.

2. **Recursion on lists**

   * Q: Implement `myLength :: [a] -> Int` recursively.
   * Hint:

     ```hs
     myLength [] = 0
     myLength (_:xs) = 1 + myLength xs
     ```
   * Answer: `myLength [1,2,3]` → `3`.

3. **Pair manipulation**

   * Q: Implement `swap :: (a, b) -> (b, a)`.
   * Hint: `swap (x,y) = (y,x)`
   * Answer: `swap (1, "a")` → `("a",1)`.

4. **Map with foldr**

   * Q: Implement `mapWithFold` using `foldr`.
   * Hint:

     ```hs
     mapWithFold f = foldr (\x acc -> f x : acc) []
     ```
   * Test: `mapWithFold (*2) [1,2,3]` → `[2,4,6]`.

5. **Filter with foldr**

   * Q: Implement `filterWithFold`.
   * Hint: see earlier in the foldr section.
   * Answer: `filterWithFold even [1,2,3,4]` → `[2,4]`.

6. **Higher-order function**

   * Q: Write `applyN :: Int -> (a -> a) -> a -> a` which applies a function `n` times.
   * Hint:

     ```hs
     applyN 0 _ x = x
     applyN n f x = f (applyN (n-1) f x)
     ```
   * Test: `applyN 3 (+2) 0` → `6` (0→2→4→6).

7. **Referential transparency check**

   * Q: Which of these are referentially transparent: `1 + 2`, `head []`, `getLine`?
   * Answer: `1 + 2` — yes; `head []` — *technically* pure but causes runtime error (so substitution changes behavior when error occurs), `getLine` — no (IO, side-effects).

### Intermediate

8. **Compose multiple steps**

   * Q: Implement `process :: [Int] -> Int` that filters evens, doubles them, and sums them.
   * Hint: `process = sum . map (*2) . filter even`
   * Test: `process [1,2,3,4]` → `2*2 + 4*2 = 4 + 8 = 12`. (Check arithmetic carefully.)

---

# 10. Quick debugging checklist (what to check when things go wrong)

* Did you match all patterns? (exhaustive pattern matching)
* Did you add a type signature? If not, add it — lots of compiler help.
* Are you mixing `:` and `++` incorrectly? `(:)` prepends element; `++` concatenates lists.
* If recursion causes stack overflow, consider tail recursion / `foldl'`.
* Use `:t` in GHCi to inspect types often.

---

### Final small challenge (try in GHCi)

Implement `pack :: Eq a => [a] -> [[a]]` that groups consecutive equal elements:

* `pack "aaaabccaadeeee"` → `["aaaa","b","cc","aa","d","eeee"]`

Hint: you’ll need recursion and pattern matching on `(x:xs)`.

---

If you want, I can:

* Solve the **pack** problem step-by-step.
* Give you a one-page printable cheat sheet with all these patterns and common functions.
* Or write a small practice set (10 problems) with solutions and GHCi commands.

Which one do you want next, Khushi?
