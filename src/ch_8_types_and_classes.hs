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

-- add :: Nat -> Nat -> Nat
-- add n m = int2nat (nat2int n + nat2int m)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

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


-- -- 8.6 Tautology checker

-- data Prop = Const Bool
--           | Var Char
--           | Not Prop
--           | And Prop Prop
--           | Imply Prop Prop
--           deriving Show

-- -- define some propositions
-- p1 :: Prop
-- p1 = And (Var 'A') (Not (Var 'A'))

-- p2 :: Prop
-- p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- p3 :: Prop
-- p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- p4 :: Prop
-- p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- type Assoc k v = [(k, v)]
-- find :: Eq k => k -> Assoc k v -> v
-- find k t = head [v | (k', v) <- t, k == k']

-- type Subst = Assoc Char Bool

-- -- eval :: Subst -> Prop -> Bool
-- -- eval _ (Const b)    = b
-- -- eval s (Var x)      = find x s
-- -- eval s (Not p)      = not (eval s p)
-- -- eval s (And p q)    = eval s p && eval s q
-- -- eval s (Imply p q)  = eval s p <= eval s q

-- vars :: Prop -> [Char]
-- vars (Const _)    = []
-- vars (Var x)      = [x]
-- vars (Not x)      = vars x
-- vars (And p q)    = vars p ++ vars q
-- vars (Imply p q)  = vars p ++ vars q

-- -- create candidate substitutions of a given length

-- -- helper functions from Ch 7:
-- type Bit = Int
-- int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- rmdups :: Eq a => [a] -> [a]
-- rmdups [] = []
-- rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- bools ::  Int -> [[Bool]]
-- -- bools n = map (reverse . map conv .make n . int2bin) range
-- --           where
-- --             range = [0 .. 2^n - 1]
-- --             make n bs = take n (bs ++ repeat 0)
-- --             conv 0 = False
-- --             conv 1 = True
-- -- Better:
-- bools 0 = [[]]
-- bools n = map (False :) bss ++ map (True :) bss
--           where bss = bools (n - 1)
            
-- substs :: Prop -> [Subst]
-- substs p = map (zip vs) (bools (length vs))
--            where vs = rmdups (vars p)

-- --isTaut :: Prop -> Bool
-- isTaut p = and [eval s p | s <- substs p]


-- 8.7 Abstract machine

data Expr = Val Int | Add Expr Expr
            deriving Show

value :: Expr -> Int
-- for Haskell-controlled evaluation:
-- value (Val n)   = n
-- value (Add x y) = value x + value y
-- for evaluation controlled as below:
value e = eval e []

type Cont = [Op]

data Op = EVAL Expr | ADD Int
          deriving Show

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

v = value (Add (Add (Val 2) (Val 3)) (Val 4)) -- = 9

{- Evaluating v by hand:
value (Add (Add (Val 2) (Val 3)) (Val 4))
= {applying value}
  eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
= {applying eval}
  eval (Add (Val 2) (Val 3)) [EVAL (Val 4)]
= {applying eval}
  eval (Val 2) [EVAL (Val 3), EVAL (Val 4)]
= {applying eval}
  exec [EVAL (Val 3), EVAL (Val 4)] 2
= {applying exec}
  eval (Val 3) [ADD 2, EVAL (Val 4)]
= {applying eval}
  exec [ADD 2, EVAL (Val 4)] 3
= {applying exec}
  exec [EVAL (Val 4)] (2 + 3)
= {applying exec}
  eval (Val 4) (ADD 5 : [])
= {applying eval}
  exec (ADD 5 : []) 4
= {applying exec}
  exec [] (5 + 4)
= {applying exec}
  9
-}

-- 8.9 Exercises

{- Exercise 1
   In a similar manner to the function add, define a recursive
   multiplication function mult :: Nat -> Nat -> Nat for the recursive
   type of natural numbers.
   Hint: make use of add in your definition.

   As a simple example, use mult for ints using addition:
         mult :: Int -> Int -> Int
         mult 1 n = n
         mult m n = n + mult (m-1) n
-}   
mult :: Nat -> Nat -> Nat
mult (Succ Zero) n = n
mult (Succ m) n = n `add` (mult m n)

-- Book's solution is better: edge case can be simplified
-- since mult is defined in terms of add
mult' :: Nat -> Nat -> Nat
mult' Zero     n = Zero
mult' (Succ m) n = n `add` (mult' m n)

{- Exercise 2

   Although not included in appendix B, the standard prelude defines
      data Ordering = LT | EQ | GT 
   together with a function 
      compare :: Ord a => a -> a -> Ordering
   that decides if one value in an ordered type is less than (LT),
   equal to (EQ), or greater than (GT) another value. Using this
   function, redefine the function occurs :: Ord a => a -> Tree a ->
   Bool for search trees. Why is this new definition more efficient
   than the original version?

   Original occurs function for search trees:
    occurs_st :: Ord a => a -> Tree a -> Bool
    occurs_st x (Leaf y)                  = x == y
    occurs_st x (Node l y r) | x == y     = True
                             | x < y      = occurs_st x l
                             | otherwise  = occurs_st x r
-}
occurs_st' :: Ord a => a -> Tree a -> Bool
occurs_st' x (Leaf y)                   = x == y
occurs_st' x (Node l y r) | ordx == EQ  = True
                          | ordx == LT  = occurs_st' x l
                          | otherwise   = occurs_st' x r
                          where ordx    = compare x y

-- Book's solution uses case expression to reduce the number of
-- comparisons to 1. Original solution could require 2, my solution
-- above could require 3.
occurs_st'' :: Ord a => a -> Tree a -> Bool
occurs_st'' x (Leaf y)     = x == y
occurs_st'' x (Node l y r) = case compare x y of
                                 LT -> occurs_st'' x l
                                 EQ -> True
                                 GT -> occurs_st'' x r

{- Exercise 3
   Consider the following type of binary trees:
      data Tree a = Leaf a | Node (Tree a) (Tree a)
   Let us say that such a tree is balanced if the number of leaves in
   the left and right subtree of every node differs by at most one,
   with leaves themselves being trivially balanced. Define a function
   balanced :: Tree a -> Bool that decides if a binary tree is
   balanced or not. Hint: first define a function that returns the
   number of leaves in a tree.
-}
data BiTree a = Leaf a | Node (BiTree a) (BiTree a)

countLeaves :: BiTree -> Int
countLeaves (Leaf a) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r
