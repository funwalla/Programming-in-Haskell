-- 8.1 Type declarations
type Pos = (Int, Int)
           
-- 8.2 Data declarations
data Move = North | South | East | West
  deriving Show

move :: Move -> Pos -> Pos
move North (x, y)  = (x, y + 1)
move South (x, y)  = (x, y - 1)
move East  (x, y)  = (x + 1, y)
move West  (x, y)  = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North  = South
rev South  = North
rev East   = West
rev West   = East

data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect l w) = l * w

-- data Maybe' a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- 8.3 Newtype declarations

-- Newtype Nat = N Int

-- 8.4 Recursive types
data Nat = Zero | Succ Nat
           deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add n m = int2nat (nat2int n + nat2int m)

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)

{-
add' (Succ (Succ Zero)) (Succ Zero)
= Succ (add' (Succ Zero) (Succ Zero))
= Succ (Succ (add' Zero (Succ Zero)))
= Succ (Succ (Succ Zero))
-}

data List a = Nil | Cons a (List a)
              deriving Show

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- a function that decides if a given value occurs in a tree:
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- flatten a tree to a list
flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- rewriting the occurs function for search trees
occurs_st :: Ord a => a -> Tree a -> Bool
occurs_st x (Leaf y)                  = x == y
occurs_st x (Node l y r) | x == y     = True
                         | x < y      = occurs_st x l
                         | otherwise  = occurs_st x r
