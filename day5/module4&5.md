## **HASKELL FOR BEGINNERS   MODULE 4 , 5**

### FUNCTION COMPOSITION

**Hook**

Think of function composition like chaining Instagram filters — each filter changes the pic and hands it to the next. In Haskell, `(.)` is that chain. Fast, tidy, and way more readable than doing everything in one messy line.



## 1. Quick definition

**Plain:** Function composition connects two functions so the output of the second becomes the input of the first.

**Type:**

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Read left-to-right: take a function `b -> c`, a function `a -> b`, and you get a function `a -> c`.


## 2. Real-life analogies 

* **Coffee assembly line:** `grind -> brew -> pour`. Compose `pour . brew . grind` to get `beans -> cup`.
* **Photoshop filters:** `sharpen . brighten . crop` = apply crop, then brighten, then sharpen.
* **Cooking:** `season . cook . chop` — you chop, then cook, then season.

Why this helps: you can think in *transformations*, not in step-by-step plumbing.


## 3. Core idea with a tiny example

```haskell
process :: [Int] -> [Int]
process = map (*2) . filter even

-- usage
process [1..6]  -- [4,8,12]
```

**Walkthrough:**

1. `filter even` keeps `[2,4,6]`.
2. `map (*2)` doubles to `[4,8,12]`.
   So `process` = apply `filter even`, then `map (*2)`.

Important: composition reads right-to-left: `map (*2) . filter even` = `\xs -> map (*2) (filter even xs)`.


## 4. Step-by-step evaluation 

Write `f . g` as `\x -> f (g x)`. Example:

```haskell
f :: Int -> Int
f x = x + 3

g :: Int -> Int
g x = x * 2

h = f . g  -- \x -> f (g x)
-- h 4 == f (g 4) == f 8 == 11
```

So `h 4` → `11`. That's the composition pipeline in action.



## 5. Compose many functions 

```haskell
toWords :: String -> [String]
toWords = words

shorten :: [String] -> [String]
shorten = filter (\w -> length w <= 4)

upperAll :: [String] -> [String]
upperAll = map (map toUpper)

pipeline :: String -> [String]
pipeline = upperAll . shorten . toWords

-- pipeline "I love Haskell and cats"
-- ["I","LOVE","CATS"] (after tokenising, filtering short words, uppercasing)
```

You chained three transformations — think of them as tiny, single-purpose workers.



## 6. Point-free style 

Instead of:

```haskell
process xs = map (*2) (filter even xs)
```

You write:

```haskell
process = map (*2) . filter even
```

Cleaner, less plumbing. But beware: point-free isn't always clearer — don’t obfuscate intent for terseness.



## 7. Order matters 

`f . g` ≠ `g . f` generally.

Example:

```haskell
f = (+1)
g = (*2)

(f . g) 3  -- (+1) (( *2) 3) = 7
(g . f) 3  -- (*2) ((+1) 3) = 8
```

So check the flow: rightmost runs first.


## 8. Use cases where composition shines

* Data pipelines (lists, streams).
* Combining small pure transformations (parsers, formatters).
* Creating readable, testable code: each function is a tiny unit you can test independently.



## 9. Mini advanced bit — composition with different shapes

Composition isn't only for functions `a -> a`. It works whenever types line up:

```haskell
show . length . words :: String -> String
-- words :: String -> [String]
-- length :: [String] -> Int
-- show :: Int -> String
```

The types must match between the chained functions.


## 10. Exercises (2 quick challenges)

1. **Predict:** What is the result of `(reverse . map succ) [1,2,3]`?

   *Solve quickly in your head.*

   
2. **Write:** Compose a function `normalize :: String -> String` that trims whitespace, lowercases, and removes punctuation. 

(Hint: use `words`, `map toLower`, `unwords`, and a `filter` removing punctuation.)



## **2. LIST COMPREHENSIONS**

### **Hook**

List comprehensions in Haskell are literally ‘SQL for lists.’ You describe what you want, Haskell does the hunting. Clean, compact, aesthetic — like writing code with a skincare routine.



## **2. What even *is* a list comprehension?**

A list comprehension is a tiny engine that:

1. loops through a list
2. filters stuff out
3. transforms each item
4. hands back the final list

All in *one* expression.

**Template:**

```haskell
[ expression | pattern <- list, condition ]
```

Read it like:

> “Give me `expression` for every `pattern` that comes from `list`, *only if* it passes these conditions.”

Exactly like set-builder notation from maths, but it actually does something instead of traumatizing you.


## **2. Core examples**

### **Basic transformation**

```haskell
[x*2 | x <- [1..5]]
-- [2,4,6,8,10]
```

Haskell’s secretly doing:
“for x in 1..5, spit out x*2.”



### **Filtering with conditions**

```haskell
[x | x <- [1..10], even x]
-- [2,4,6,8,10]
```

SQL vibes:

```sql
SELECT x FROM numbers WHERE even(x);
```


## **3. Real-life analogies**

* **Shopping filter:** “Give me all T-shirts under ₹500 that are black.”
* **Spotify filter:** “Give me all songs longer than 3 minutes that match my mood.”
* **Cafeteria:** “Give me all samosas that aren’t burnt.”

Haskell just automates your preferences.



## **4. Multiple generators = nested loops but prettier**

```haskell
[(x, y) | x <- [1,2,3], y <- [4,5]]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
```

This secretly does:

* loop x over [1,2,3]
* for each x, loop y over [4,5]
* produce all combos

Basically a Cartesian product without losing your sanity.

Real life:
Picking outfits — shirts × pants = all combos.



## **5. Guards — conditions you slap inside**

```haskell
[x | x <- [1..10], x*x > 30]
-- [6,7,8,9,10]
```

Guards = vibes check.
If x doesn’t pass the test, it’s kicked out of the list.
No drama, no warnings — it just never shows up again.

## **6. Slightly fancier example**

Let’s say you want the **squares of odd numbers between 1 and 20**:

```haskell
[x*x | x <- [1..20], odd x]
-- [1,9,25,49,81,121,169,225,289,361]
```

Wanna keep only squares above 100?

```haskell
[x*x | x <- [1..20], odd x, x*x > 100]
-- [121,169,225,289,361]
```

Bro it’s literally “Stack filters until you get the list you vibe with.”



## **7. Pattern matching inside comprehensions**

You can split items while looping.

Example: Grab names from a list of `(name, age)` tuples where age ≥ 18:

```haskell
[name | (name, age) <- [("Ana", 17), ("Mehul", 21), ("Riya", 19)], age >= 18]
-- ["Mehul","Riya"]
```

Clean and specific — like searching your WhatsApp chats for “bro send notes.”



## **8. List comprehensions with multiple transformations**

Let’s get all even numbers from 1–30, divide them by 2, and convert to strings:

```haskell
[show (x `div` 2) | x <- [1..30], even x]
```

Pipeline, but horizontally.



## **9. Using multiple conditions**

```haskell
[x | x <- [1..50], even x, x > 10, x `mod` 3 /= 0]
```

Breakdown:

* even
* greater than 10
* not divisible by 3

Feels like configuring a Tinder filter but with better results.



## **10. Mini practical demo — building a quick “SQL-like” query**

### Problem:

Take a list of students and select the names of those who scored above 80.

```haskell
students = [("Aman", 78), ("Sana", 92), ("Rohan", 61), ("Kriti", 88)]

topStudents = [name | (name, score) <- students, score > 80]
-- ["Sana","Kriti"]
```

You literally wrote a SELECT query in Haskell.



## **11. Bonus — multiple generators with conditions**

Create all pairs (x, y) where x from 1–5, y from 1–5, but only keep pairs where x < y:

```haskell
[(x, y) | x <- [1..5], y <- [1..5], x < y]
-- [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]
```

This is how you generate combinations without repetition.



## **12. Quick rules of thumb (closing)**

* Start with the list → add filters → add transformation.
* If your logic feels like “give me all items that satisfy…”, use a comprehension.
* Multiple generators = nested loops.
* Guards are your filtering friends.
* Keep the expression on the left simple — that’s your output, not your logic.



## **3. ANONYMOUS (LAMBDA) FUNCTIONS**

### **Hook**

Sometimes you just wanna do one quick calculation and move on with your life. Naming a whole function for that? Nah. Lambdas are the ‘I’ll handle it right here’ energy.


## **1. What is a lambda?**

A **lambda** is a **function with no name**.
You write it, use it, toss it.
Haskell literally uses `\` to mimic the λ symbol.

### **Syntax**

```haskell
\x -> x * 2
```

Read it like:

> “Take an x and return x * 2.”

Very short-term commitment. No need to define a full `f x = ...`.


## **2. Why do people actually use lambdas?**

* You need a tiny function once.
* Not worth naming.
* Keeps code focused.
* Aesthetic points — fewer lines, less boilerplate.

Think of it like grabbing a paper cup for one sip instead of washing a whole mug.


## **3. Basic example**

```haskell
map (\x -> x + 1) [1,2,3]
-- [2,3,4]
```

Equivalent to:

```haskell
map addOne [1,2,3]
where addOne x = x + 1
```

But why name a whole function just to increment once?


## **4. Real-life analogies**

* **Disposable cup:** Use → toss → forget.
* **Quick note scribble:** You don’t open a Google Doc for a 5-second reminder.
* **Instant seasoning:** You sprinkle chilli powder once; you don’t rename the spoon “SpiceFunction1.”

Lambdas are temporary tools.


## **5. Multiple arguments**

Lambdas can have as many parameters as you want:

```haskell
\x y -> x + y
```

Used like:

```haskell
(\x y -> x + y) 10 20
-- 30
```

Behind the scenes, it’s the same as:

```haskell
add = \x -> \y -> x + y
```

Currying still applies — lambdas don’t get special treatment.


## **6. Lambdas inside functions (super common)**

### Filtering only big values:

```haskell
filter (\n -> n > 50) nums
```

### Sorting with a custom rule:

```haskell
sortBy (\a b -> compare (length a) (length b)) strings
```

### Mapping to pairs:

```haskell
map (\x -> (x, x*x)) [1..5]
-- [(1,1),(2,4),(3,9),(4,16),(5,25)]
```

Lambdas shine wherever the logic is ultra-specific.


## **7. Pattern-matching inside lambdas (yep, allowed)**

```haskell
map (\(a,b) -> a + b) [(1,2), (3,4)]
-- [3,7]
```

Just like regular functions — but lightweight.


## **8. Lambda with guards (chef’s kiss)**

You can even do guards if you wrap them:

```haskell
\x -> case x of
        n | n > 10    -> "big"
          | otherwise -> "small"
```

Not super common, but good to know.


## **9. When NOT to use lambdas (important)**

If the logic is longer than 1–2 lines, or used multiple times,
**give it a name**.
Otherwise your code starts looking like a cryptic escape room puzzle.

Bad vibes version:

```haskell
map (\x -> let y = x*x in if y > 100 then y + 3 else y - 4) nums
```

At that point just write a normal function.


## **10. Small real-world mini demo**

Say you’re processing a list of users and want only usernames in uppercase:

```haskell
users = ["khushi", "arjun", "meera"]

cleaned = map (\name -> map toUpper name) users
-- ["KHUSHI","ARJUN","MEERA"]
```

Nobody wants to define `toUpperName` for this.


## **11. Another practical one: compute total price**

```haskell
sum (map (\(qty, price) -> qty * price) cart)
```

You’re not naming:

```haskell
costOfItem (qty, price) = qty * price
```

— because it’s literally used once.


## **12. Quick lightning-round syntax tips**

* `\x -> ...` = one argument
* `\x y -> ...` = two arguments
* `\(a,b) -> ...` = pattern match
* Lambdas *always* end at the first `->`


## **4. LIBRARIES**

### **Hook**

Haskell libraries are like power-ups. You start with the Prelude — the default starter pack — but the moment you need something fancy, you just import the right module and boom: superpowers unlocked.


## **1. What even is a library/module?**

A **module** is just a box of functions and types someone already wrote so you don’t have to.
You import it → you get access → you stop reinventing nonsense.

Haskell’s **Prelude** is loaded by default:

* `map`
* `filter`
* `foldr`
* `length`
* `take`
* `maybe`, `either`, etc.

But beyond that, the ecosystem goes *crazy*: text processing, JSON, math, maps, HTTP, concurrency, GUIs — all sitting there waiting for you to say:

```haskell
import Some.Module
```


## **2. Import basics**

The syntax is extremely chill:

```haskell
import Data.List
import Data.Char (toUpper)
```

* If you import the whole module → you get everything in it
* If you import a specific thing → you only get that

Feels like selectively shopping instead of buying the whole mall.



## **3. Examples**

### **Sorting a list**

```haskell
sort [3,1,2]    -- [1,2,3]
```

But this works only if you imported:

```haskell
import Data.List
```

### **Uppercasing everything**

```haskell
map toUpper "haskell"
-- "HASKELL"
```

Comes from:

```haskell
import Data.Char (toUpper)
```

No need to write your own uppercase function like a caveman.



## **4. Qualified imports**

Sometimes two modules have functions with the same name (`map`, `lookup`, `insert`, etc.).
To avoid the *“bro which lookup are you calling??”* situation, you use qualified imports:

```haskell
import qualified Data.Map as M
```

Now you call things like:

```haskell
M.lookup "key" M.empty
```

It’s clean, readable, and avoids namespace beef.



## **5. Hiding or exposing specific stuff**

If a module has something with a name that clashes with yours, you can hide it:

```haskell
import Data.List hiding (filter)
```

Or expose *only* what you want:

```haskell
import Data.Set (fromList, toList)
```

Think of it like curating your playlist — keep what fits your vibe, skip the rest.


## **6. Real-life analogy (fun but accurate)**

Libraries are like **plugins** or **add-ons**.
You don’t:

* build your own database engine
* write your own sort function
* code your own ASCII table
* implement HTTP from scratch

You *import* it, use it, move on with life.



## **7. Practical mini-demo**

Let’s say you want to count word frequencies in a text.

You’ll probably use:

```haskell
import Data.Char (toLower, isAlphaNum)
import qualified Data.Map as M
```

Then:

```haskell
wordFreq :: String -> M.Map String Int
wordFreq text =
    foldl (\m word -> M.insertWith (+) word 1 m)
          M.empty
          (words (map toLower (filter isAlphaNumOrSpace text)))
  where
    isAlphaNumOrSpace c = isAlphaNum c || c == ' '
```

Boom.
Haskell libraries handled the heavy lifting — you just orchestrated.



## **8. Commonly used modules**

* `Data.List` → sort, group, partition, intercalate
* `Data.Char` → uppercase/lowercase, digit checks
* `Data.Map` → efficient key-value storage
* `Data.Set` → unique collections
* `Data.Maybe` → helpers for Maybe values
* `Text.JSON` / `Aeson` → JSON encoding/decoding
* `Control.Monad` → monadic utilities
* `System.Random` → random numbers

Trust me, 70% of Haskell code is just picking the right modules.



## **9. How Haskell finds libraries**

Anything built into the platform comes from "base".
Anything external comes from:

* **Hackage** (Haskell’s library universe)
* **Cabal** or **Stack** (package managers)

You install, then import. Easy.


## **14. COMPUTATIONAL EFFECTS**

### **Hook**

"Pure functions are great and all, but real programs don’t live in a math bubble. They read stuff, write stuff, throw tantrums (exceptions), generate randomness, talk to files — basically everything professors told you functions shouldn’t do."



## **1. What are computational effects?**

Effects = things your program does that aren’t just “input → output”.

Real-world examples:

* Printing to screen
* Reading user input
* Generating random numbers
* File I/O
* Network requests
* Updating memory
* Throwing errors
* Concurrency

In Haskell, these are *controlled*. You don’t just YOLO side effects like in JavaScript.



## **2. Why?**

Haskell wants purity. Effects break purity.
Solution? Wrap effects inside **types** (`IO`, `Maybe`, `Either`, etc.) and control the chaos.

Think of it like:
You don’t stop your friend from being chaotic, you just don’t let them do it unsupervised.



## **3. The big idea**

An *effect type* describes:

* the actions taken
* the final value returned

Example:

```haskell
getLine :: IO String
```

Meaning:

> “Do some I/O, then eventually give me a String.”

Effects are *sequenced*, not free-for-all.


## **15. I/O PRACTICE — DO NOTATION**

### **Hook**

"Here’s the cheat code: I/O looks imperative, but it’s actually pure under the hood. Do-notation is your ‘fake imperative mode’ so your brain doesn’t overheat."


## **1. Basic do-block structure**

```haskell
do
  line1
  line2
  ...
  lineN
```

Each line is either:
✔ an IO action
✔ a binding `x <- action`
✔ a pure let-binding `let x = expr`

---

## **2. Example: reading input**

```haskell
main = do
  putStrLn "Your name?"
  name <- getLine
  putStrLn ("Hello " ++ name)
```

This feels imperative but is actually a chain of pure descriptions of effects.


## **3. Example: infinite loop using recursion**

```haskell
loop = do
  putStr "Type something: "
  s <- getLine
  putStrLn ("You typed: " ++ s)
  loop
```

Haskell is the only language where people flex recursion for loops, but it works.


## **4. Example: countdown with condition**

```haskell
countdown n = do
  print n
  if n == 0
    then putStrLn "Liftoff!"
    else countdown (n-1)
```

It’s literally an if-statement — don’t overthink it.



## **5. Example: random numbers**

```haskell
import System.Random (randomRIO)

randomUntilZero = do
  x <- randomRIO (0,9)
  print x
  if x == 0
    then return ()
    else randomUntilZero
```

Feels like a slot machine.


## **16. I/O TYPES**

### **Hook**

"`IO a` looks cryptic but it’s not. It’s literally: 'an action that, when run, produces a value of type a'. That’s it. No black magic, no Hogwarts."


## **1. Core concept**

`IO a` = *a sequence of actions* + *final return value type*.

Examples:

```haskell
putStrLn "Hi" :: IO ()
getLine       :: IO String
return 99     :: IO Int
readFile f    :: IO String
```


## **2. Why does `putStrLn` return `()`?**

Because it *does* something but doesn’t *produce* anything.

Unit `()` = “nothing meaningful here, move along”.


## **3. In a do-block**

If you write:

```haskell
x <- getLine
```

Then:

* `getLine :: IO String`
* `x :: String`

If you write:

```haskell
let y = 10
```

Then:

* pure binding, no IO involved
* `y :: Num t => t` depending on usage

## **4. `return` is not a return statement**

In Haskell:

```haskell
return x
```

Just wraps `x` in IO: `IO x`.
It does NOT exit the function like Python or Java.




