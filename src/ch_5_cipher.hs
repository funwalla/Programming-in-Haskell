import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise =  c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2,
         0.8, 4.0, 2.4, 6.7,  7.5, 1.9, 0.1, 6.0, 6.3, 9.0,
         2.8, 1.0, 2.4, 0.2,  2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: [Char] -> Int
lowers cs = length [c | c <- cs,
                        c >= 'a' && c <= 'z']
count :: Char -> [Char] -> Int
count c' cs = length [c | c <- cs, c == c']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
                    where n = lowers xs
                          
