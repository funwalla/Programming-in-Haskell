import Data.Char
import Data.List

-- 5 List comprehensions

-- 5.1 Basic concepts
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

first :: [(a,b)] -> [a]
first xs = [x | (x, _) <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- 5.2 Guards
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

find' :: Eq a => a -> [(a,b)] -> [b]
find' k kvs = [v | (k', v) <- kvs, k' == k]

-- 5.3 The zip function
adjPairs :: [a] -> [(a,a)]
adjPairs xs = zip xs (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = and [x  <= y | (x, y) <- adjPairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 .. ], x' == x]

-- 5.4 String comprehensions
lowers :: [Char] -> Int
lowers cs = length [c | c <- cs, c >= 'a' && c <= 'z']

count :: Char -> [Char] -> Int
count c cs = sum [1 | c' <- cs, c' == c]

-- 5.5 The Caesar cipher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((n + let2int c) `mod` 26)
    | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n cs = [shift n c| c <- cs ]              

-- Frequency tables

-- for the alphabet:
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- for individual strings:
--percent
percent :: Int -> Int -> Float
percent n m = (fromIntegral n  /  fromIntegral m) * 100

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a' .. 'z']]
             where n = (lowers cs)

-- χ-squared statistic:
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ (observed - expected)^2 / expected
                   | (observed,  expected) <- zip os es
                   ]

rotate :: Int -> [a] -> [a]
rotate n cs = drop n cs ++ take n cs

crack :: String -> String
crack xs = encode (-factor) xs
             where factor = head (positions (minimum chitab) chitab)
                   chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
                   table' = freqs xs

-- 5.7 Exercises

{-  Exercise 1
    Using a list comprehension, give an expression that calculates
    the sum of 1^2 + 2^2 + ... + 100^2 of the first 100 integer squares
-}
sqrs :: Int -> Int
sqrs n = sum [k^2 | k <- [1 .. n]]

{-  Exercises 2.
    Suppose that a coordinate grid of size m × n is given by the list
    of all pairs (x, y) of integers such that
        0 <= x <= m and 0 <= y <= n.
    Using a list comprehension, define a function 
        grid :: Int -> Int -> [(Int,Int)] 
    that returns a coordinate grid of a given size.
-}
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

{-  Exercise 3.
    Using a list comprehension and the function grid above, define a
    function square :: Int -> [(Int,Int)] that returns a coordinate
    square of size n, excluding the diagonal from (0, 0) to (n, n).
-}
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

{-  Exercise 4.
    In a similar way to the function length, show how the library
    function replicate :: Int -> a -> [a] that produces a list of
    identical elements can be defined using a list comprehension.
-}
replicate' :: Int -> a -> [a]
replicate' n a = [a | i <- [1 .. n]]

{-  Exercise 5.
    A triple (x, y, z) of positive integers is Pythagorean if it
    satisfies the equation x^2 + y^2 = z^2. Using a list comprehension
    with three generators, define a function pyths :: Int ->
    [(Int,Int,Int)] that returns the list of all such triples whose
    components are at most a given limit.
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n],
                        y <- [1 .. n],
                        z <- [1 .. n],
                        x^2 + y^2 == z^2]

{-  Exercise 6
    A positive integer is perfect if it equals the sum of all of its
    factors, excluding the number itself. Using a list comprehension
    and the function factors, define a function 
        perfects :: Int -> [Int]
    that returns the list of all perfect numbers up to a given limit.
-}
perfects :: Int  -> [Int]
perfects n = [i | i <- [1 .. n], sum (init (factors i)) == i]

{-  Exercise 7
    Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]]
    with two generators can be re-expressed using two comprehensions
    with single generators. Hint: nest one comprehension within the
    other and make use of the library function concat :: [[a]] -> [a].
-}
answer7 = concat [ [(1, m) | m <- [3,4] ], [(2, n) | n <- [3,4]] ]

{-  Exercise 8
    Redefine the function positions using the function find.
-}
positions' :: Eq a => a -> [a] -> [Int]          
positions' a as = find' a (zip as [0 .. ])

{-  Exercise 9
    The scalar product of two lists of integers xs and ys of length n
    is given by the sum of the products of corresponding integers:
        Σ xs(i) * ys(i), where i runs from 0 to n-1
    In a similar manner to chisqr, show how a list comprehension can
    be used to define a function 
        scalarproduct :: [Int] -> [Int] -> Int
    that returns the scalar product of two lists.
-}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys =  sum [x * y | (x, y) <- zip xs ys]

{-  Exercise 10
    Modify the Caesar cipher program to also handle 
    upper-case letters.
-}
