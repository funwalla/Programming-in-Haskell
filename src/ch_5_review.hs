
import Data.Char

-- 5.1 Basic concepts

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts xs = [a | (a, _) <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- 5.2 Guards

factors :: Int -> [Int]
factors n = [k | k <- [1 .. n], n `mod` k == 0]

numFactors :: Int -> Int
numFactors = length . factors

prime :: Int -> Bool
--prime n = length (factors n) == 2
-- book's answer takes advantage of lazy evaluation:
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [k | k <- [2 .. n], prime k]

find :: Eq k => k -> [(k, v)] -> [v]
find k' kvs = [v | (k, v) <- kvs, k == k']

-- 5.3 The zip function

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

l = [1,2,3,2,4,6,5,7,2]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..])

-- 5.4 String comprehensions

-- requires Data.Char, imported at top of file

lowers :: [Char] -> Int
lowers cs = sum [1 | c <- cs, isLower c]
-- book's solution avoids importing Data.Char:
lowers' :: [Char] -> Int
lowers' cs = length [c | c <- cs,
                         c >= 'a' && c <= 'z']

count :: Char -> [Char] -> Int
count c' cs = sum [1 | c <- cs, c == c']
-- book: countChar c' cs = length [c | c <- cs, c == c']

-- 5.5 The Caesar cipher

-- n.b., Data.Char imported at top of file

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c
          
encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4,  6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0,  2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n d = (fromIntegral n / fromIntegral d) * 100

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a' .. 'z']]
               where n = lowers cs
