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

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

This looks like it takes *two* arguments, but actually:
`add` takes one argument (`x`) and returns **a new function** that takes the next (`y`).
This is called **currying**.

So:

```haskell
(add 5) 3
```

is equivalent to `5 + 3`.

 

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

A program is **referentially transparent** if an expression can be replaced by its value without changing behavior.

 

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

 

###  Benefit:

Referential transparency allows:

* Easier reasoning about code
* Easier debugging
* Safe parallelization and caching

This is one of Haskell’s biggest strengths.

 

##  TOPIC 7: PAIRS

###  Tuples in Haskell

A **pair** is a fixed-size group of values.

```haskell
pair :: (Int, String)
pair = (42, "Answer")
```

Access with pattern matching:

```haskell
first (x, _) = x
second (_, y) = y
```

**Example usage:**

```haskell
first (10, "hi")   -- 10
second (10, "hi")  -- "hi"
```

Compare to:

* Java → `Pair<Integer, String>`
* Python → tuple unpacking `(a, b) = (10, "hi")`

 

##  TOPIC 8: LISTS

Lists are *the backbone* of Haskell programming.

 

###  Definition

A list in Haskell is written in brackets:

```haskell
nums = [1,2,3,4,5]
letters = ['a','b','c']
```

 

###  Operations

```haskell
head [1,2,3]      -- 1
tail [1,2,3]      -- [2,3]
last [1,2,3]      -- 3
init [1,2,3]      -- [1,2]
```

The `:` operator (“cons”) adds an element to the front:

```haskell
0 : [1,2,3]  -- [0,1,2,3]
```

 

###  Recursion with Lists

```haskell
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

Try:

```haskell
sumList [1,2,3]  -- 6
sumList [1, 2, 3]
= 1 + sumList [2, 3]
= 1 + (2 + sumList [3])
= 1 + (2 + (3 + sumList []))
= 1 + (2 + (3 + 0))
= 6

```

Haskell programmers think “head + tail”, not “index and loop”.

 

##  TOPIC 9: HIGHER-ORDER FUNCTIONS

### Definition

A higher-order function is one that takes another function as an argument or returns one.

 

###  `map`

```haskell
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
```

Try:

```haskell
map' (*2) [1,2,3]   -- [2,4,6]
```

 

###   `filter`

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs
```

Try:

```haskell
filter' even [1,2,3,4,5]  -- [2,4]
```

 

###   `foldr` (Intro)

```haskell
foldr (+) 0 [1,2,3]   -- 6
foldr (++) "" ["Ha","sk","ell"]  -- "Haskell"
```

`foldr` combines elements using a function and an initial value  , think “reduce” in Python.

 

##   TOPIC 10: CODE QUALITY

###   Core Principles:

1. **Explicit type signatures**  , act like documentation
2. **Small, composable functions**  , easier to test and reuse
3. **Avoid side effects**  , stay pure when possible
4. **Use meaningful names**
5. **Leverage function composition**

 

###   Example

```haskell
processNumbers :: [Int] -> [Int]
processNumbers = map (+1) . filter even
```

Equivalent to:

```haskell
processNumbers xs = map (+1) (filter even xs)
```

The `.` is function composition  , elegant, readable, and purely functional.

 

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
