-- 6.1 Basic concepts
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

(.*.) :: Int -> Int -> Int
m .*. 0 = 0
m .*. n = m + m .*. (n - 1)

-- 6.2 Recursion on lists
-- product' :: [Int] -> Int
-- product' []     = 1
-- product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

(.++.) :: [a] -> [a] -> [a]
[]     .++. ys = ys
(x:xs) .++. ys = x : xs .++. ys

insert' :: Ord a => a -> [a] -> [a]
insert' x []                  = [x]
insert' x (y:ys) | x <= y     = x : y : ys
                 | otherwise  = y : insert' x ys

isort :: Ord a  => [a] -> [a]
isort []     = []
isort (x:xs) = insert' x (isort xs)

-- 6.3 Multiple arguments

zip' :: [a] -> [b] -> [(a, b)]
zip' _  [] = []
zip' [] _  = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- drop' :: Int -> [a] -> [a]
-- drop' 0 xs      = xs
-- drop' _ []      = []
-- drop' n (_:xs)  = drop' (n-1) xs

-- 6.4 Multiple recursion

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = smaller ++ [x] ++ larger
                 where
                   smaller = qsort [x' | x' <- xs, x' < x]
                   larger  = qsort [x' | x' <- xs, x' >= x]

-- 6.5 Mutual recursion

even' :: Int -> Bool
even' 0 = True
even' n = odd (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n - 1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- 6.6 Advice on recursion  
-- Example: function to find the product of a list of numbers

-- Step 1: Define the type
-- product :: [Int] -> Int

-- Step 2: Enumerate the cases
-- product [] = 
-- product (n:ns) =

-- Step 3: Define the simple cases
-- product [] = 1

-- Step 4: Define the other cases
-- product (n:ns) = n * product ns

-- Step 5: Generalize and simplify
-- product should handle any numeric type:
product' :: Num a => [a] -> a
product' []      = 1
product' (x:xs)  = x * product' xs

-- Example: drop n items from the front of a list
-- Step 1: Define the type
drop' :: Int -> [a] -> [a]
-- Step 2: Enumerate the cases
-- drop' _ [] =
-- drop' 0 xs = 
-- drop' n (x:xs) = 
-- Step 3: Define the simple cases
drop' 0 xs = xs
drop' _ [] = []
-- Step 4: Define the other cases
drop' n (_:xs) = drop' (n-1) xs
-- Step 5: Generalize and simplify

-- Example: init: removes the last element of a non-empty list
init' :: [a] -> [a]
init' []      = []
init' [_]     = []
init' (x:xs)  = x : init' xs

-- 6.8 Exercises

{-  Exercise 1
    How does the recursive version of the factorial function behave if
    applied to a negative argument, such as (-1)? 
        The original definition of the factorial function will go into
        an infinite loop if applied to a negative argument.
    Modify the definition to prohibit negative arguments by adding a
    guard to the recursive case.

-}
fac' :: Int -> Int
fac' 0              = 1
fac' n | n > 0      = n * fac' (n-1)
       | otherwise  = error "factorial is not defined for negative arguments."

{-  Exercise 2
    Define a recursive function  sumdown :: Int -> Int  that returns the
    sum of the non-negative integers from a given value down to
    zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.
-}
sumdown :: Int -> Int
sumdown 0              = 0
sumdown n | n > 0      = n + sumdown (n-1)
          | otherwise  = error "sumdown is not defined for negative arguments."

{-  Exercise 3
    Define the exponentiation operator ^ for non-negative integers
    using the same pattern of recursion as the multiplication operator
    *, and show how the expression 2 ^ 3 is evaluated using your
    definition.
-}
(.^.) :: Int -> Int -> Int
_ .^. 0              = 1
m .^. n | n > 0      = m * (m .^. (n-1))
        | otherwise  = error "Negative exponents not allowed."

{-  Exercise 4
    Define a recursive function  euclid :: Int -> Int -> Int  that
    implements Euclid’s algorithm for calculating the greatest common
    divisor of two non-negative integers: if the two numbers are
    equal, this number is the result; otherwise, the smaller number is
    subtracted from the larger, and the same process is then repeated.
-}
euclid :: Int -> Int -> Int
euclid m n | n == m     = m
           | m < n      = euclid m (n - m)
           | otherwise  = euclid (m - n) m

{-  Exercise 5
    Using the recursive definitions given in this chapter, show how
    length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.

    length :: [a] -> Int
    length []     = 0
    length (x:xs) = 1 + length xs

    length [1,2,3] = 1 + length [2,3] = 1 + (1 + length [3])
                   = 1 + (1 + (1 + length [])) = 1 + (1 + (1 + 0))
                   = 1 + (1 + 1) = 1 + 2 = 3

    drop :: Int -> [a] -> [a]
    drop 0 xs      = xs
    drop _ []      = []
    drop n (x:xs)  = drop (n - 1) xs
    
    drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5] = drop 1 [3,4,5]
                       = drop 0 [4,5] = [4,5]

    init :: [a] -> [a]
    init [_] = []
    init (x:xs) = x : init xs

    init [1,2,3] = 1 : init [2,3] = 1 : (2 : init [3]) = 1 : (2 : [])
                 = 1 : [2] = [1,2]
-}

{-  Exercise 6

    Without looking at the definitions from the standard prelude,
    define the following library functions on lists using
    recursion. 
-}
-- a.Decide if all logical values in a list are True: 
and' :: [Bool] -> Bool
and' []                   = True
and' (b:bs) | b == False  = False
            | otherwise   = and' bs
-- This solution is concise but at the price of classifying
-- an empty list as True.           

-- b.Concatenate a list of lists:
cat :: [a] -> [a] -> [a]
cat [] ys = ys
cat (x:xs) ys = x: cat xs ys

concat' :: [[a]] -> [a]
concat' [xs]      = xs
concat' (xs:xss)  = cat xs (concat' xss)

-- concat' [[1], [2,3], [4,5]] = cat [1] (concat' [[2,3], [4,5]])
--       = cat [1] (cat [2,3] (concat' [[4,5]]))
--       = cat [1] (cat [2,3] [4,5]) = cat [1] (2 : cat [3] [4,5])
--       = cat [1] (2 : (3 : cat [] [4,5]))
--       = cat [1] (2 : (3 : [4,5])) = cat [1] (2 : [3,4,5])
--       = cat [1] [2,3,4,5] = 1 : cat [] [2,3,4,5] = 1 : [2,3,4,5]
--       = [1,2,3,4,5]

-- c.Produce a list with n identical elements: 
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- d.Select the nth element of a list: 
(.!!.) :: [a] -> Int -> a
(x:xs) .!!. 0                = x
(x:xs) .!!. n
    | n < 0                  = error "negative index"
    | n > length (x:xs) - 1  = error "index too large"
    | otherwise              = xs .!!. (n - 1)
                
-- e.Decide if a value is an element of a list: 
elem' :: Eq a => a -> [a] -> Bool
elem' _ []                  = False
elem' x (y:ys) | x == y     = True
               | otherwise  = elem' x ys

{-    Note: most of these functions are defined in the prelude using
    other library functions rather than using explicit recursion, and
    are generic functions rather than being specific to the type of
    lists.
-}

{-  Exercise 7
    Define a recursive function merge :: Ord a => [a] -> [a] -> [a]   
    that merges two sorted lists to give a single sorted list.  
        For example: merge [2,5,6] [1,3,4] [1,2,3,4,5,6]
    Note: your definition should not use other functions on sorted
    lists such as insert or isort, but should be defined using
    explicit recursion.
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs []                      = xs
merge [] ys                      = ys
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys

{-  Exercise 8
    Using merge, define a function msort :: Ord a => [a] -> [a] that
    implements merge sort, in which the empty list and singleton lists
    are already sorted, and any other list is sorted by merging
    together the two lists that result from sorting the two halves of
    the list separately. Hint: first define a function halve :: [a] ->
    ([a],[a]) that splits a list into two halves whose lengths differ
    by at most one.
-}
halve :: [a] -> ([a],[a])
halve [] = ([], [])
halve xs = (take half xs, drop half xs)
             where half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []   = []
msort [x]  = [x]
msort xs   = merge ( msort (fst half)) (msort (snd half))
               where half = halve xs
{-
msort [6,2,8,5] = merge (msort [6,2]) (msort 8, 5)
                = merge (merge (msort [6]) (msort [2])) 
                        (merge (msort [8]) (msort [5]))
                = merge (merge [6] [2]) (merge [8] [5])
                = merge [2,6] [5,8] = [2,5,6,8]
-}

{- Exercise 9
    Using the five-step process, construct the library functions that:
-}

-- a. calculate the sum of a list of numbers
sum' :: Num a => [a] -> a
sum' []      = 0
sum' (x:xs)  = x + sum' xs

-- b. take a given number of elements from the start of a list
take' :: Int -> [a] -> [a]
take' _ []                  = []
take' n (x:xs) | n > 0      = x : take' (n-1) xs
               | otherwise  = []

-- c. select the last element of a non-empty list
last' :: [a] -> a
last' []                       = error "empty list"
last' (x:xs) | length xs == 0  = x
             | otherwise       = last' xs
