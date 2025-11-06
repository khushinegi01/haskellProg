# **HASKELL FOR BEGINNERS   DAY 2**



## **Topics**

* Lists (in-depth)
* Constructors
* Pattern Matching
* Guards
* Type Classes (Ad-hoc Polymorphism)
* Map
* Partial Application
* Where-clauses
* Practice Exercises

## **INTRODUCTION**

Welcome back!
Yesterday, you got your first taste of how Haskell *thinks*   recursion instead of loops, functions as pure transformations, and a type system that’s rock solid.

Today, we go deeper.

You’ll move from *thinking recursively* to *structuring programs elegantly* using **pattern matching, guards, lazy evaluation**, and **type classes**   tools that make Haskell expressive, reusable, and beautifully concise.

## **1. LISTS (REVISITED)**

We already saw lists briefly on Day 1   ordered collections of elements of the same type:

```haskell
nums = [1,2,3,4,5]
```

But now, let’s understand what’s *really happening* under the hood.

### **Structure**

A list is either:

1. **Empty list** `[]`, or
2. **An element followed by another list**, written as `x : xs`.

So `[1,2,3]` is just shorthand for:

```haskell
1 : (2 : (3 : []))
```

The `:` (called the *cons operator*) builds lists from the front.

You can only “add” elements at the front with `:`   not at the end (that’s slow).


### **Head and Tail**

To get the first element and the rest:

```haskell
head [1,2,3]   -- 1
tail [1,2,3]   -- [2,3]
```

### **Empty List Check**

You can test for emptiness:

```haskell
null []      -- True
null [1,2]   -- False
```

##  2. CONSTRUCTORS

A **constructor** is a function (or symbol) that **creates a value** of some data type.

Every data type in Haskell is defined **in terms of its constructors**.
They’re the *building blocks* of values.


###  Example 1: Boolean type

```haskell
data Bool = True | False
```

Here:

* `True` and `False` are **constructors**.
* They are the only possible *values* of type `Bool`.

So if you write:

```haskell
flag = True
```

you’re **constructing** a Boolean value using the constructor `True`.


###  Example 2: Lists

Lists are built from two constructors:

1. `[]` — the **empty list constructor**
2. `(:)` — the **cons constructor** (it adds an element to the front)

So `[1,2,3]` is just shorthand for:

```haskell
1 : (2 : (3 : []))
```

Each `(:)` joins one element to the rest of the list — a bit like linking nodes in a chain.

---

###  Think of it like this:

| Code     | Meaning                          | Analogy                       |
| -------- | -------------------------------- | ----------------------------- |
| `[]`     | Empty list                       | End of chain                  |
| `x : xs` | Attach `x` to front of list `xs` | Link `x` to the rest of chain |

So:

```haskell
5 : 4 : 3 : 2 : 1 : []
```

is a chain of links ending in `[]`.

If you draw it:

```
 5 -> 4 -> 3 -> 2 -> 1 -> []
```

Each arrow (`->`) is like the `:` constructor connecting one node to the next.

---

### ⚙️ Type of the constructors

You can check their types in GHCi:

```haskell
Prelude> :t []
[] :: [a]

Prelude> :t (:)
(:) :: a -> [a] -> [a]
```

So:

* `[]` creates an **empty list** of any element type.
* `(:)` takes one element (`a`) and a list of elements (`[a]`) and produces a new list (`[a]`).

That’s why `5 : [4,3,2,1]` works — it’s `a -> [a] -> [a]`.


 **Key idea:**
Constructors *build* data values by following the structure defined in the type.
You can think of them as “data factories.”


## **3. PATTERN MATCHING**

##  Pattern Matching on Lists

Let’s look at this function:

```haskell
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
```

Let’s unpack that carefully.


### Step 1: Two possible shapes of a list

A list has only **two forms** (because of the two constructors):

1. `[]` — empty
2. `x : xs` — a head element `x` and the rest `xs`

So the function defines **one equation per constructor**.


### Step 2: First pattern — the base case

```haskell
length' [] = 0
```

If the list matches the empty pattern `[]`, the result is `0`.


### Step 3: Second pattern — recursive case

```haskell
length' (_:xs) = 1 + length' xs
```

If the list is non-empty, it must look like `(x:xs)`:

* The head is `x`
* The tail is `xs`

The `_` means “I don’t care about this value” (so we don’t name it).

Then we count `1` (for the head) plus the length of the tail (recursively).


### Step 4: How Haskell matches patterns

When you call:

```haskell
length' [10,20,30]
```

Haskell actually sees:

```haskell
length' (10 : 20 : 30 : [])
```

Now it tries to match the first pattern:

* `[]`? ❌ No, doesn’t match.
* `(_:xs)`? ✅ Yes! So `_` = 10, `xs` = [20,30].

It substitutes those and continues:

```
1 + length' [20,30]
```

Repeats until `[]` matches, giving:

```
1 + (1 + (1 + 0)) = 3
```

## **4. GUARDS**

Sometimes, pattern matching isn’t enough   you also want *conditions* on values.
That’s where **guards** come in.


### **Syntax:**

```haskell
function pattern
  | condition1 = result1
  | condition2 = result2
  | otherwise  = default
```


### **Example: Sign Function**

```haskell
sign :: Int -> String
sign n
  | n > 0     = "Positive"
  | n < 0     = "Negative"
  | otherwise = "Zero"
```

Guards read like natural language   they’re literally “guarding” which result runs.


### Combine pattern matching + guards (very common)

```haskell
describeList :: [a] -> String
describeList []      = "Empty list"
describeList (x:[])  = "Single element list"
describeList (x:y:_) = "List with more than one element"
```

Add guards to refine behavior inside a pattern:

```haskell
describeNums :: [Int] -> String
describeNums [] = "Empty"
describeNums xs
  | sum xs == 0 = "Non-empty but sums to zero"
  | length xs > 5 = "Long list"
  | otherwise = "Short non-zero-list"
```

Here the second equation handles *all non-empty lists* (pattern `xs`) and then the guards narrow down which message to return.


## **5. LAZY EVALUATION**

Haskell’s laziness means it doesn’t evaluate an expression until it *must*.
You saw this in Day 1’s infinite list demo, but let’s push it further.

---

### **Example: Infinite List**

```haskell
nums = [1..]         -- infinite list
take 5 nums          -- [1,2,3,4,5]
```

Haskell won’t compute `[1,2,3,...]` all at once   it only generates what’s needed.

---

### **Why Laziness Matters**

1. **You can define infinite data structures.**
2. **You avoid unnecessary computation.**

---

**Example:**

```haskell
firstSquareOver100 = head [x*x | x <- [1..], x*x > 100]
```

Even though `[1..]` is infinite, Haskell stops once it finds the first match.

Lazy evaluation + recursion = clean and efficient data flow.

---

## **6. TYPE CLASSES (Ad-hoc Polymorphism)**

You already know **parametric polymorphism** from Day 1   `identity :: a -> a` works for *any* type.

Now we add **constraints**: functions that work for *some* types that share common behavior.

---

### **What’s a Type Class?**

Type classes are one of Haskell’s most important and useful features. They let you write *ad-hoc polymorphic* functions: the same function name can work for different types, but only for types that agree to implement certain behavior.

Think of type classes like **interfaces** in OOP (Java/C#): a type that is an *instance* of a type class promises to provide implementations for the operations that the class describes.

* **Parametric polymorphism** — “works for any type”:

  ```haskell
  identity :: a -> a
  identity x = x
  ```

  `identity` behaves the same for all types — it doesn’t assume anything about `a`.

* **Ad-hoc polymorphism (type classes)** — “works for many types that share some behaviour”:

  ```haskell
  isEqual :: Eq a => a -> a -> Bool
  isEqual x y = x == y
  ```

  `isEqual` needs the ability to test equality, so it only works for types that are instances of `Eq`.

##  Using type classes: constraints in type signatures

When a function needs a capability provided by a type class, you put a **constraint** before the `=>` in the type signature:

```haskell
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
```

Read it as: *“For all types `a` that are instances of `Eq`, `isEqual` has type `a -> a -> Bool`.”*

Multiple constraints are comma-separated:

```haskell
combine :: (Show a, Eq a) => a -> String
```


##  Common built-in type classes

A few very important ones (you’ll see these everywhere):

* `Eq` — equality: `(==)`, `(/=)`
* `Ord` — ordering: `(<)`, `(<=)`, `(>)`, `compare`
* `Show` — convert to `String`: `show`
* `Read` — parse from `String`: `read`
* `Num` — numeric operations: `+`, `-`, `*` (and more)
* `Functor`, `Applicative`, `Monad` — for structures that can be mapped over / combined (more advanced)

Many standard types like `Int`, `Bool`, `Char`, lists `[a]`, tuples, etc., already have instances for these classes.


##  Instances — how a type *implements* a class

If you define a new data type, you make it an instance of a class by writing an `instance` block.

### Example: a simple `Color` type

```haskell
data Color = Red | Green | Blue

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"
```

Now `show Red` returns `"Red"`.

### Example: custom `Eq` instance

```haskell
instance Eq Color where
  Red == Red     = True
  Green == Green = True
  Blue == Blue   = True
  _ == _         = False
```

Now you can compare colors with `==`.


## Default method implementations & minimal definition

Type classes can provide **default implementations** for some methods. For example `(/=)` can default to `not (x == y)` if you define `==`. When you make an `instance`, you only need to implement the minimal set required by the class.

Example (informal):

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)  -- default
```

If you implement `==` in your instance, you automatically get `/=` unless you override it.



##  How dispatch works (at a high level)

When you call `show someValue` the compiler figures out the concrete type of `someValue` and uses the `Show` instance for that type. This resolution is done at compile time — there is no runtime virtual dispatch like in OOP.


##  Examples that show why type classes are handy

### Sorting generic lists — requires `Ord`:

```haskell
minOfTwo :: Ord a => a -> a -> a
minOfTwo x y = if x <= y then x else y
```

This works for `Int`, `Char`, `String`, or any `a` that has an `Ord` instance.

### Pretty printing — `Show`:

```haskell
printIt :: Show a => a -> IO ()
printIt x = putStrLn (show x)
```

### Combining constraints:

```haskell
compareAndShow :: (Ord a, Show a) => a -> a -> String
compareAndShow x y
  | x == y    = show x ++ " is equal to " ++ show y
  | x < y     = show x ++ " is less than " ++ show y
  | otherwise = show x ++ " is greater than " ++ show y
```


##  Type class vs. inheritance — the difference

* Inheritance (OOP): a subtype “is-a” specialization of a base class and inherits implementation.
* Type class (Haskell): a type “implements” a set of functions; multiple types can become instances of the same class. There’s no implicit data sharing or object identity. Type classes describe *capabilities*, not object layout.


##  Common errors and gotchas

* **Missing constraint**: If you write `f x = x == x` but `x`’s type has no `Eq` constraint, compiler will ask for one. Add `Eq a =>` in the type signature.
* **Overlapping instances / orphan instances**: Defining instances in the wrong module or writing conflicting instances can cause problems. Keep instances near the type or the class definition when possible.



##  Small exercises (try them)

1. Write `isSorted :: Ord a => [a] -> Bool` that checks if a list is non-decreasing.
2. Define a `data Shape = Circle Float | Rect Float Float` and make it an instance of `Show` (pretty-print).
3. Define a type class `CanBeZero` with function `isZero :: a -> Bool` and make instances for `Int` and `Float`.




## **7. PARTIAL APPLICATION**

You learned in Day 1 that every function technically takes **one argument** at a time.

Partial application uses that to create new specialized functions.

---

### **Example**

```haskell
add :: Int -> Int -> Int
add x y = x + y

add5 :: Int -> Int
add5 = add 5
```

Now `add5 10` → `15`.

`add 5` didn’t perform the addition   it *returned a function* that knows how to add 5.

---

### **Operator Sections**

You can partially apply operators too:

```haskell
(>3) :: Int -> Bool
map (>3) [1,2,3,4,5]  -- [False,False,False,True,True]
```

Partial application makes functional pipelines short and powerful.

---

## **9. WHERE CLAUSES**

When your function has helper logic, **where clauses** keep it neat.

---

### **Syntax**

```haskell
function args = expression
  where
    helper1 = ...
    helper2 = ...
```

---

### **Example**

```haskell
circleArea :: Float -> Float
circleArea r = pi * square r
  where
    square x = x * x
```

Here, `square` is a helper visible only inside `circleArea`.

---

### **Example with Guards + Where**

```haskell
bmiTell :: Float -> Float -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Overweight"
  | otherwise   = "Obese"
  where
    bmi = weight / height ^ 2
```

Without `where`, you’d repeat the BMI calculation in every guard   messy and repetitive.

---

## **10. PRACTICE EXERCISES**

Now it’s time to practice everything from this module   based on the `2-Lists` and `2B-Map` tutorials.

---

### **Exercise 1: Sum of Elements**

Write a recursive function that adds up all elements of a list.

```haskell
sum' :: [Int] -> Int
```

---

### **Exercise 2: Maximum Element**

Find the largest number in a list.

```haskell
maximum' :: Ord a => [a] -> a
```

---

### **Exercise 3: Count Occurrences**

Count how many times a specific element appears.

```haskell
count :: Eq a => a -> [a] -> Int
```

---

### **Exercise 4: Filter with Guards**

Use guards to return only even numbers.

```haskell
onlyEven :: [Int] -> [Int]
```

---

### **Exercise 5: Map Practice**

Using `map`, write:

A function to add 10 to every element.


---

### **Exercise 6: Nested Map**

Use `map (map f)` to add 1 to each number in a list of lists:

```haskell
map (map (+1)) [[1,2,3],[4,5]]
```

---

### **Exercise 7: Where Clause Challenge**

Write a BMI calculator using guards and a `where` clause (as shown above).

---

### **Exercise 8: Combine It All**

Build a function that:

1. Takes a list of integers.
2. Filters out the odds.
3. Doubles the evens.
4. Returns the new list.

```haskell
processList :: [Int] -> [Int]
processList = map (*2) . filter even
```

---

## **SUMMARY**

| Concept                 | What It Means                             | Key Idea                  |
| ----------------------- | ----------------------------------------- | ------------------------- |
| **Lists**               | Recursive sequences of same-type elements | Built with `[]` and `(:)` |
| **Constructors**        | Build data types                          | `x : xs` for lists        |
| **Pattern Matching**    | Deconstruct data                          | Match `[]`, `(x:xs)`      |
| **Guards**              | Add conditions to patterns                | Cleaner than `if/else`    |
| **Type Classes**        | Shared behavior for types                 | `Eq`, `Ord`, `Num`, etc.  |
| **Map**                 | Apply function to all elements            | `(a -> b) -> [a] -> [b]`  |
| **Partial Application** | Fix one arg to make new fn                | `add5 = add 5`            |
| **Where Clauses**       | Define helpers locally                    | Keeps code clean          |

---

## **CLOSING REMARKS**

You’ve just unlocked a massive part of Haskell’s power   understanding how its list processing, pattern matching, and laziness combine into elegant, declarative code.

From here, Haskell stops looking weird and starts looking *clean.*

Next up: we’ll go beyond lists into **Data types ,RPN calculator ,Compiler structure , Accumulators, Tail recursion , Computational complexity ,Global and local names**


