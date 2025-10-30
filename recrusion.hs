-- Factorial --
factorial :: Integer -> Integer

factorial 0 = 1
factorial n = n * factorial (n-1) 


main = do 
    putStrLn "Factorial of your number "
    print(factorial 50)



-- ##  Step 1: What is a Factorial?

-- 
-- * `3! = 3 × 2 × 1 = 6`
-- * `1! = 1`
-- * By definition, `0! = 1`

-- ---
-- The **factorial** of a number `n` (written as `n!`) is defined as:


--      n! = n × (n - 1) × (n - 2) × … × 1


-- For example:

-- * `5! = 5 × 4 × 3 × 2 × 1 = 120`
-- ##  Step 2: Recursive idea

-- Recursion means **a function calls itself** to solve a smaller version of the same problem.

-- So we can define factorial recursively as:

-- 
-- n! = n × (n - 1)!
-- 

-- And the smallest possible case (the **base case**) is:

-- 0! = 1


-- ##  Step 3: Factorial Program in Java

-- ```java
-- public class FactorialExample {
--     // Recursive method
--     static int factorial(int n) {
--         if (n == 0) {           // Base case
--             return 1;
--         } else {
--             return n * factorial(n - 1);  // Recursive call
--         }
--     }

--     public static void main(String[] args) {
--         int number = 5;
--         int result = factorial(number);
--         System.out.println("Factorial of " + number + " is: " + result);
--     }
-- }
-- ```

-- ###  Output:

-- ```
-- Factorial of 5 is: 120
-- ```

-- ---

-- ##  Step 4: Step-by-step Execution for `factorial(5)`

-- | Function Call | What it Does           | Returned Value         |
-- | ------------- | ---------------------- | ---------------------- |
-- | factorial(5)  | `5 * factorial(4)`     | waits for factorial(4) |
-- | factorial(4)  | `4 * factorial(3)`     | waits for factorial(3) |
-- | factorial(3)  | `3 * factorial(2)`     | waits for factorial(2) |
-- | factorial(2)  | `2 * factorial(1)`     | waits for factorial(1) |
-- | factorial(1)  | `1 * factorial(0)`     | waits for factorial(0) |
-- | factorial(0)  | returns `1`            | base case              |
-- | factorial(1)  | returns `1 * 1 = 1`    |                        |
-- | factorial(2)  | returns `2 * 1 = 2`    |                        |
-- | factorial(3)  | returns `3 * 2 = 6`    |                        |
-- | factorial(4)  | returns `4 * 6 = 24`   |                        |
-- | factorial(5)  | returns `5 * 24 = 120` | ✅ Final Answer         |

-- ---

-- ##  Step 5: Flowchart (Visual Aid)

-- Here’s a simple flowchart to visualize the recursion process:

-- ```plaintext
--           ┌─────────────────────┐
--           │   Start (main)      │
--           └───────┬─────────────┘
--                   │
--                   ▼
--         ┌─────────────────────┐
--         │  Call factorial(n)  │
--         └───────┬─────────────┘
--                 │
--         ┌───────▼──────────────┐
--         │ Is n == 0 ?          │
--         └───────┬──────────────┘
--            Yes  │      No
--                 │
--    ┌────────────▼───────────┐
--    │ Return 1 (base case)   │
--    └────────────┬───────────┘
--                 │
--           ┌─────▼──────────────────────┐
--           │ Return n * factorial(n-1)  │
--           └───────────┬────────────────┘
--                       │
--                       ▼
--               ┌──────────────┐
--               │   End/Return │
--               └──────────────┘
-- ```

