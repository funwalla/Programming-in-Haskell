import Data.Char

-- 7.1 Basic concepts

-- functions can be written to show that they can return functions:
add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

-- functions may take functions as arguments:
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2 Processing lists

-- map defined using a list comprehension:
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- map defined using recursion:
map'' :: (a -> b) -> [a] -> [b]
map'' f []      = []
map'' f (x:xs)  = f x : map'' f xs

-- filter defined using a list comprehension:
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                  = []
filter'' p (x:xs) | p x        = x : filter'' p xs
                  | otherwise  = filter'' p xs

-- using filter and map together to define a function that returns the
-- sum of the squares of the even integers from a list:
sum_even_sqrs :: [Int] -> Int
sum_even_sqrs xs = sum (map (^2) (filter even xs))

-- 7.3 The foldr function

-- General pattern:
-- f [] = v              v = the starting value
-- f(x:xs) - x # f xs    # = the operation

-- Mnemonic: replace the cons with # and the null list with v.

sum_foldr :: Num a => [a] -> a
sum_foldr = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

-- Defining foldr using recursion:
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []      = v
foldr' f v (x:xs)  = f x (foldr f v xs)

-- application method: replace each cons of the list by the function
--                     and the final empty list by the starting value.
-- e.g., foldr (+) 0 [1, 2] = foldr (+) 0 (1 : 2 : []) = 1 + 2 + 0

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

-- so length' [1,2,3] = 1 + (1 + (1 + 0))

length'' :: [a] -> Int
length'' = foldr (\_ n -> n + 1) 0

-- length'' ['a', 'b', 'c']
--  = (\_ n -> n + 1) 'a' ( (\_ n -> n + 1) 'b' ( (\_ n -> n + 1) 'c' 0 ) )
--  = (\_ n -> n + 1) 'a' ( (\_ n -> n + 1) 'b' 1  )
--  = (\_ n -> n + 1) 'a' 2 = 3

reverse' :: [a] -> [a]
reverse' []      = []
reverse' (x:xs)  = reverse' xs ++ [x]

-- reverse' [1,2,3] = (reverse' [2,3]) ++ [1]
--                  = ((reverse' [3]) ++ [2]) ++ [1]
--                  = (((reverse' []) ++ [3]) ++ [2]) ++ [1]
--                  = (([] ++ [3]) ++ [2]) ++ [1]    ① 

-- define the anticons:
snoc x xs = xs ++ [x]
snocTest  = snoc 1 (snoc 2 (snoc 3 [])) -- from ① above
-- so
reverse'' :: [a] -> [a]
reverse'' = foldr snoc []

-- foldr reflects the use of an operator that associates to the right.
--     e.g., foldr (+) 0 [1,2,3] = 1 + (2 + (3 + 0))
-- or more generally:
--   foldr (#) v [x0, x1 .. xn] = x0 # (x1 # (... (xn # v)...))

-- 7.4 The foldl function     (for left-associating operators)

sum' :: Num a  => [a] -> a
sum' = s 0
        where
          s v []      = v
          s v (x:xs)  = s (v + x) xs

-- sum' [1,2,3] = s 0 [1,2,3] = s (0 + 1) [2,3]
--   = s ((0 + 1) + 2) [3] = s (((0 + 1) + 2) + 3) [] = 6

-- f v [] = v
-- f v (x:xs) = f (v # x) xs

sum_l :: Num a => [a] -> a
sum_l = foldl (+) 0

or_l :: [Bool] -> Bool
or_l = foldl (||) False

and_l :: [Bool] -> Bool
and_l = foldl (&&) True

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- 7.5 The composition operator

-- one definition:
(<.>) :: (b -> c) -> (a -> b) -> (a -> c)
f <.> g = \x -> f (g x)

odd' :: Int -> Bool
--odd' n = not (even n)
odd' = not . even

--sumsqreven :: [Int] -> Int
--sumsqreven ns = sum [n^2 | n <- ns, even n]
--sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven = sum . map (^2) . filter even

-- 7.6 Binary string transmitter

type Bit = Int

-- bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--                  where weights = iterate (*2) 1

{- Simpler definition of bin2int:
Let [a, b, c, d] represent an arbitrary four-bit binary number.
Then bin2int [a, b, c, d] = 1 * a + 2 * b + 4 * c + 8 * d
                          = a + 2 * b + 4 * c + 8 * d
                          = a + 2 * (b + 2 * c + 4 * d)
                          = a + 2 * (b + 2 * (c + 2 * d))
                          = a + 2 * (b + 2 * (c + 2 * (d + 2 * 0)))
                          = foldr (\x y -> x + 2 * y) 0 [a, b, c, d]
so bin2int may be defined as:
-}
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
-- make8 bs
--     | length bs >= 8  = take 8 bs
--     | otherwise       = bs ++ replicate (8 - length bs) 0
--  better:
make8 bs = take 8 (bs ++ repeat 0)

{-
We can now use map and function composition to define a function that
encodes a string of characters as a list of bits by
    - converting each character into a Unicode number,
    - converting each such number into an eight-bit binary number,
    - and concatenating each of these numbers together to produce a
      list of bits.
-}
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)
{-
Define a function that decodes a list of bits as a string of
characters by chopping the list up, and converting each resulting
binary number into a Unicode number and then a character:
-}
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8 (drop 8 bs)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- 7.7 Voting algorithms

