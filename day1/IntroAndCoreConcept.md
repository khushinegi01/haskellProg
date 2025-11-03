##  HASKELL FOR BEGINNERS  , DAY 1

### Topics:

* Recursion
* Termination
* Types
* Polymorphism
* Functions & Function Types
* Referential Transparency
* Pairs
* Lists
* Higher-Order Functions
* Code Quality


##  INTRODUCTION 

> If you’ve used **Java** or **Python**, you’re used to giving your computer a list of *instructions*  , do this, then that, then loop until done.
> Haskell is different. It’s a **purely functional** language, so instead of “how to do it”, you describe *what something is*.
>
> This changes how we write code, how we reason about code, and even how we debug it.
> It might feel strange at first, but by the end of this lesson, you’ll start to see why functional programming is elegant and powerful.

###  **Introduction to Haskell**

> “So what makes Haskell special?
> Let’s go over a few key features.”

1. **Purely Functional Language**

   > Haskell treats computation as the evaluation of *expressions* rather than a sequence of *commands.*
   > In Java or Python, you often change variables or states. In Haskell , once a value is set, it doesn’t change.
   > This means *no side effects*, and your functions behave like real mathematical functions.

   ```haskell
   add :: Int -> Int -> Int
   add x y = x + y
   ```

   > That’s a function that *always* returns the same output for the same inputs , no surprises.


2. **Lazy Evaluation**

   > Haskell doesn’t compute results until it actually *needs* them.
   > This is called *lazy evaluation*, and it allows for infinite data structures and efficient computation.

   ```haskell
   numbers = [1..]       -- infinite list
   firstFive = take 5 numbers   -- [1,2,3,4,5]
   ```

   > In Python, that would blow up memory.
   > In Haskell, it works beautifully because values are computed *only when required.*


3. **Statically Typed (with Type Inference)**

   > Haskell checks types *before* running your program, which catches many bugs early ,
   > but you don’t always need to write the types yourself. The compiler infers them.

   ```haskell
   square x = x * x
   -- Haskell infers: square :: Num a => a -> a
   ```

   > Compare this with Java, where you must declare every type explicitly.


4. **Modular and Composable**

   > Haskell programs are built using *small, reusable* pure functions.
   > You can compose them easily to build larger programs , think of it like Lego blocks.

   ```haskell
   evenSquare x = (square . (*2)) x
   -- same as square (*2 x)
   ```

   > This compositional nature leads to cleaner, more maintainable code , and that’s where functional programming shines.

##  TOPIC 1: RECURSION

###  Concept

> In Haskell, **recursion** replaces the loops you’re used to in Java and Python.
> Instead of saying “repeat X times”, we say “define the result in terms of a smaller version of itself”.

Every recursive function has two essential parts:

1. A **base case** (when to stop)
2. A **recursive case** (how to reduce the problem)

 

###  Example 1: Sum of Natural Numbers

**Java version:**

```java
int sum(int n) {
    int s = 0;
    for (int i = 1; i <= n; i++) {
        s += i;
    }
    return s;
}
```

**Python version:**

```python
def sum(n):
    s = 0
    for i in range(1, n + 1):
        s += i
    return s
```

**Haskell version:**

```haskell
sumUp :: Int -> Int
sumUp 0 = 0
sumUp n = n + sumUp (n - 1)
```

**Explanation:**

* The **base case** `sumUp 0 = 0` means we stop when `n` reaches 0.
* The **recursive case** `sumUp n = n + sumUp (n - 1)` means: “sum of n numbers = n + sum of (n-1) numbers”.

**So,**

```
sumUp 3 = 3 + sumUp 2
        = 3 + (2 + sumUp 1)
        = 3 + (2 + (1 + sumUp 0))
        = 3 + 2 + 1 + 0
```

 

###  Example 2: Factorial

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

Same pattern:
Base case → when `n` is 0.
Recursive case → multiply `n` by factorial of smaller `n`.

In Java/Python you might use loops, but in Haskell **recursion feels natural**. It’s literally how the language is meant to think.

 

###  Thought Break:

> 🗣️ “But won’t this blow up the stack?”
> Good question. Haskell is *lazy*  , it doesn’t evaluate until it must.
> We’ll later learn about **tail recursion** and **lazy evaluation**, but for now, trust that this simple form works beautifully for small inputs.

 

##  TOPIC 2: TERMINATION

###  Why talk about this?

Because recursion is powerful  , but without a **stopping condition**, it’s infinite.

Example of *non-terminating* recursion:

```haskell
loopForever :: Int -> Int
loopForever n = loopForever n
```

Run this, and it’ll hang forever (or crash).
Haskell can’t “force stop” an infinite recursion  , it just keeps building the call stack lazily.

So every recursive definition must:

* Have a **base case** (the simplest version)
* Move **closer** to that base case each time

Example:

```haskell
countDown :: Int -> [Int]
countDown 0 = [0]
countDown n = n : countDown (n - 1)
```

 

###  Compare to Java:

In Java, a for-loop like `for(int i=n; i>=0; i--)` has the “termination” built into the loop header.
In Haskell, **you write it explicitly**.

That’s why thinking recursively trains your brain to design functions differently.

 

##  TOPIC 3: TYPES

###  Static and Strong Typing

In Python, types are dynamic  , you can add an int and a string and it crashes *at runtime*.
In Haskell, everything has a **type** known at **compile time**, preventing errors early.

```haskell
sumUp :: Int -> Int
```

This means: “sumUp takes an Int and returns an Int”.

If you accidentally pass a string:

```haskell
sumUp "Hello"
```

The compiler instantly says  “Type mismatch! Expected Int, got String”.

 

###  Type Inference

You don’t *always* need to specify the type:

```haskell
double x = x * 2
```

Haskell infers `x :: Num a => a -> a`.
This means: for any type `a` that’s numeric, it’ll work  , like Int, Float, Double, etc.

 

###  Type Signatures  , Your Friend

They’re optional but highly recommended for clarity.
Always write them like function documentation.

 

##  TOPIC 4: POLYMORPHISM

###  Meaning:

Polymorphism = “many forms”.
In Java, that usually means class inheritance; in Haskell, it means **type generalization**.

 

###  Example: The Identity Function

**Java Generic:**

```java
<T> T identity(T x) {
    return x;
}
```

**Haskell:**

```haskell
identity :: a -> a
identity x = x
```

The lowercase `a` is a *type variable* (like Java’s `<T>`).
So `identity` works for *any* type.

Try in GHCI:

```haskell
identity 10     -- 10
identity "Hi"   -- "Hi"
identity True   -- True
```

That’s **parametric polymorphism**  , functions that work the same way regardless of type.

 

###  Example: A Polymorphic List Length

```haskell
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
```

This works for `[Int]`, `[String]`, `[Bool]`  , anything!

 

##  TOPIC 5: FUNCTIONS AND FUNCTION TYPES

###  Everything is a Function
In Haskell every function takes exactly one argument and returns exactly one result. When you see something like `Int -> Int -> Int`, it’s actually `Int -> (Int -> Int)`: a function that returns another function. This is **currying**.


In Haskell, **functions are first-class citizens**.
That means you can:

* Pass them as arguments
* Return them from other functions
* Store them in data structures

 

###  Basic Example

```haskell
addOne :: Int -> Int
addOne x = x + 1
```


Simple. But now look at:

### The code:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

Looks like `add` takes *two numbers* and gives you a sum, right?
But actually, in Haskell, **functions only take one thing at a time**.

Here’s what’s really going on:

* `add 5` doesn’t *add* yet , it gives you a **new function** that *knows* how to add 5 to whatever number you give it next.

Think of it like this:

 **Analogy:**
Imagine `add` is a “sum machine.”
When you give it one number, say 5, it *remembers* that number and turns into a half-ready machine waiting for the second number.

So:

* `add 5` → a machine that’s waiting for another number to complete the job.
* `(add 5) 3` → feeds 3 into that machine → boom, 8.

Or if you want it super visual:

```
add      :: takes x -> returns (function waiting for y)
add 5    :: that waiting function (Int -> Int)
(add 5) 3 :: actually does 5 + 3 = 8
```

So Haskell is like: “I don’t rush, bro. One input at a time.”
That’s what **currying** is — every function technically takes one argument and returns another function until it has all it needs.

### Why it’s useful

* You can create specialized functions easily: `add5 = add 5` — now `add5 10` → 15.
* Functions are easy to compose and pass around.

 

###  Function as Argument

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

Try:

```haskell
applyTwice (*2) 3   -- 12
applyTwice reverse "abc"  -- "abc"
```

This is the functional programming superpower  , manipulating functions *like data*.

 

##  TOPIC 6: REFERENTIAL TRANSPARENCY

###  Definition

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

###  Example

```haskell
square x = x * x
```

`square 3` → `9`.
No side-effects, no hidden state.
Wherever you see `square 3`, you can replace it with `9` safely.

 

###  Contrast with Python/Java:

```python
import random
def f(): return random.randint(1,10)
```

Every call gives a different result → **not** referentially transparent.

Or:

```java
int count = 0;
int inc() { return ++count; }
```

Calling `inc()` changes state  , not pure.

 

### Benefits

* Easier reasoning: no hidden surprises.
* Safer parallelism: computations can run in any order.
* Memoization/caching becomes possible.

A program is **referentially transparent** if an expression can be replaced by its value without changing behavior.

This is one of Haskell’s biggest strengths.

 

##  TOPIC 7: PAIRS

###  Tuples in Haskell

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


Compare to:

* Java → `Pair<Integer, String>`
* Python → tuple unpacking `(a, b) = (10, "hi")`

 

##  TOPIC 8: LISTS

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


Haskell programmers think “head + tail”, not “index and loop”.

##  TOPIC 9: HIGHER-ORDER FUNCTIONS

###  What is a Higher-Order Function?

A **higher-order function (HOF)** is basically a function that:

* **takes another function as an input**, or
* **returns a new function as output**,
  or both.

Think of them as “function manipulators.”
Normal functions work with data. Higher-order functions work with *functions themselves*.

Example :

> If normal functions are chefs that cook food, higher-order functions are managers who train and direct chefs to cook differently.


## 1️ map – *Transform a list*

```haskell
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
```

###  Breakdown

* `(a -> b)` = a function that converts one type to another.
  Example: `(*2)` turns `Int` to `Int`, or `show` turns `Int` to `String`.
* `[a]` = list of inputs
* `[b]` = list of outputs

So `map'` says:
“Give me a function and a list, and I’ll apply that function to *every element* of the list.”

###  Example

```haskell
map' (*2) [1,2,3] 
-- f = (*2)
-- Step 1: apply f to 1 → 2
-- Step 2: apply f to 2 → 4
-- Step 3: apply f to 3 → 6
-- Result: [2,4,6]
```

###  Analogy

Think of `map` as an assembly line.
Each item on the belt (the list element) passes through a machine (the function) that modifies it before moving to the output tray.

---

##  filter – *Pick only what matches*

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs
```

###  Breakdown

* `(a -> Bool)` = a test function (predicate).
* It keeps only the items that pass the test (return `True`).

###  Example

```haskell
filter' even [1,2,3,4,5]
-- p = even
-- 1 -> False -> skip
-- 2 -> True -> keep
-- 3 -> False -> skip
-- 4 -> True -> keep
-- 5 -> False -> skip
-- Result: [2,4]
```

###  Analogy

Imagine a bouncer at a club checking IDs.
Everyone (list elements) walks up, the bouncer (`p`) says “Yes” or “No,” and only those who pass get in.


##  foldr – *Combine everything into one*

```haskell
foldr (+) 0 [1,2,3]       -- 6
foldr (++) "" ["Ha","sk","ell"]  -- "Haskell"
```

###  Breakdown

`foldr` takes:

1. a function `(a -> b -> b)`
2. a starting value (base case)
3. a list
   and it **reduces** the whole list to one value by combining elements step by step from the **right**.

###  Example 1

```haskell
foldr (+) 0 [1,2,3]
= 1 + (2 + (3 + 0))
= 6
```

The `0` is like your “starting point.”
The `+` is the operation used to combine things.

###  Example 2

```haskell
foldr (++) "" ["Ha","sk","ell"]
= "Ha" ++ ("sk" ++ ("ell" ++ ""))
= "Haskell"
```

###  Analogy

Imagine you’re stacking dominoes from right to left, and every time you place one, you combine it with what’s already fallen.
That “combining rule” is your function (like `+` or `++`), and the “ground” it falls onto is your starting value (like `0` or `""`).



## `foldl` – Fold Left

### Definition:

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
```

Compare with `foldr`:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

They look similar, right?
The only difference is **the order** they apply the function in.


###  How it works:

`foldl` takes:

1. a **combining function** `(b -> a -> b)`
2. an **initial value** (starting point)
3. a **list**

And it **reduces** the list from *left to right*.


###  Example:

```haskell
foldl (+) 0 [1,2,3]
```

Here’s how it goes step-by-step:

```
foldl (+) 0 [1,2,3]
= foldl (+) (0 + 1) [2,3]
= foldl (+) (1 + 2) [3]
= foldl (+) (3 + 3) []
= 6
```

So the combination happens **left-to-right**, like:

```
(((0 + 1) + 2) + 3)
```

Whereas `foldr` would go:

```
(1 + (2 + (3 + 0)))
```

Both give the same result here, but they *process differently*.


### Analogy:

Think of folding a long paper strip:

* `foldl` folds it **from the start to the end**.
* `foldr` folds it **from the end to the start**.

The end result *might* be the same, but the path there differs.


###  When It Matters:

With **non-symmetric** operations (like `:`, subtraction, or string concat), order changes the result.

Example:

```haskell
foldl (-) 0 [1,2,3]
-- (((0 - 1) - 2) - 3) = -6

foldr (-) 0 [1,2,3]
-- (1 - (2 - (3 - 0))) = 2
```

 Totally different.



###  Quick Recap Table:

| Function | Direction    | Combines Like       | Example Result (with `+`) |
| -------- | ------------ | ------------------- | ------------------------- |
| `foldr`  | Right → Left | `1 + (2 + (3 + 0))` | `6`                       |
| `foldl`  | Left → Right | `((0 + 1) + 2) + 3` | `6`                       |


**Pro tip:**
`foldl` can be more memory efficient if you use `foldl'` (the strict version) — otherwise, lazy evaluation can stack up thicc thunks (intermediate unevaluated expressions).

Want me to show how `foldr` vs `foldl` behave *differently* on an infinite list? That’s where things get spicy 

##  Summary

| Function | Type                             | What it Does                    | Analogy                                 |
| -------- | -------------------------------- | ------------------------------- | --------------------------------------- |
| `map`    | `(a -> b) -> [a] -> [b]`         | Transforms every element        | Factory machine that upgrades each item |
| `filter` | `(a -> Bool) -> [a] -> [a]`      | Keeps elements that pass a test | Bouncer picking who gets in             |
| `foldr`  | `(a -> b -> b) -> b -> [a] -> b` | Reduces a list to one value     | Domino chain collapsing into one result |





 

##   TOPIC 10: CODE QUALITY

###  Explicit Type Signatures

### Definition

Every function should start with a **type signature** — like this:

```haskell
processNumbers :: [Int] -> [Int]
```

That line says:

> “Hey, this function takes a list of Ints and gives you back another list of Ints.”

###  Why it matters

* It’s **self-documenting** : you can tell what a function does without even reading its body.
* It **catches bugs early** : if your logic doesn’t fit the declared type, Haskell yells at you before you even run the code.
* It **helps future devs (or future you)** understand intent fast.

Think of it like writing *function contracts* , no surprises later.


##  Small, Composable Functions

### Definition

Break big logic into **tiny, focused** pieces that each do *one thing well*.
Then glue them together.

Example:

```haskell
increment :: Int -> Int
increment x = x + 1

filterEven :: [Int] -> [Int]
filterEven = filter even

processNumbers :: [Int] -> [Int]
processNumbers = map increment . filterEven
```

###  Why it matters

* Each piece is dead simple to test.
* You can reuse them in other contexts.
* Easier to debug and maintain.

Think of it like **LEGO blocks** : one block does a small job, but you can combine them to build anything.


##  Avoid Side Effects (Stay Pure)

### Definition

A **pure function** is one that:

* Always gives the same output for the same input.
* Doesn’t mess with the outside world (no printing, no I/O, no modifying global vars).

Example:

```haskell
pureAdd :: Int -> Int -> Int
pureAdd x y = x + y
```

vs

```haskell
impureAdd :: Int -> IO ()
impureAdd x = print (x + 1)
```

###  Why it matters

Purity = predictability.
You can test, reason about, and refactor pure functions safely.

Haskell actually *forces* you to keep side effects wrapped in `IO`, so you know exactly where the “real-world” stuff happens.


##  Use Meaningful Names

### Definition

Names should reflect **what the function does**, not how it does it.

Bad:

```haskell
f x = x + 1
```

Good:

```haskell
increment x = x + 1
```

Better:

```haskell
increaseScore :: Int -> Int
increaseScore score = score + 1
```

###  Why it matters

You don’t want future-you (or your team) guessing what `f`, `foo`, or `bar` means at 2 AM.
Good names save mental energy and make reading code effortless.


##  Leverage Function Composition (`.`)

### Definition

`(.)` is the **function composition operator**.
It lets you combine functions into a clean pipeline, right-to-left.

```haskell
processNumbers = map (+1) . filter even
```

This means:

> “First filter the even numbers, then map (+1) over the result.”

Equivalent to:

```haskell
processNumbers xs = map (+1) (filter even xs)
```

But composition is cleaner — no repeating `xs`, less clutter.


###  Analogy

Imagine chaining filters in Photoshop:

* `filter even` → select the right pixels
* `map (+1)` → brighten them

Using `.` is like building that chain once instead of nesting parentheses everywhere.
It’s Haskell’s version of writing *beautiful, declarative pipelines.*


##  Example Breakdown

### With Composition

```haskell
processNumbers :: [Int] -> [Int]
processNumbers = map (+1) . filter even
```

**Pipeline:**

1. `filter even` → keeps only even numbers.
2. `map (+1)` → adds 1 to each of those numbers.
3. Done.

Run it:

```haskell
processNumbers [1,2,3,4,5,6]
-- Step 1: [2,4,6]
-- Step 2: [3,5,7]
-- Result: [3,5,7]
```

### Without Composition

```haskell
processNumbers xs = map (+1) (filter even xs)
```

Same logic, just uglier syntax.


##  The Core Idea

Write code that:

* Reads like a *description*, not instructions.
* Has minimal noise and zero side effects.
* Feels like **math equations**, not imperative steps.


##  Summary Table

| Principle                | Meaning                          | Why It’s                         |
| ------------------------ | -------------------------------- | ---------------------------------- |
| **Type Signatures**      | Describe what your function does | Acts as built-in docs + bug filter |
| **Small Functions**      | One job per function             | Easier to test, reuse, and reason  |
| **Avoid Side Effects**   | Keep logic pure                  | Makes behavior predictable         |
| **Meaningful Names**     | Name shows purpose               | Improves readability instantly     |
| **Function Composition** | Chain small functions            | Elegant, concise, expressive       |
 

##  CLOSING REMARKS

> Congratulations! You’ve just made it through your first serious Haskell session.
> You now understand:
>
> * How recursion replaces loops
> * Why termination matters
> * How Haskell’s type system works
> * What polymorphism and pure functions look like
> * How lists, pairs, and higher-order functions are the heart of Haskell
>
> In the next session, we’ll explore **pattern matching, guards, type classes, and modules**.
