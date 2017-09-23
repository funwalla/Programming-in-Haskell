-- 9.1 Introduction

{-
Given a sequence of numbers and a target number, attempt to construct
an expression whose value is the target, by combining one or more
numbers from the sequence using addition, subtraction, multiplication,
division and parentheses.
-}

-- 9.2 Arithmetic operators

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _  = True
valid Sub x y  = x > y
valid Mul _ _  = True
valid Div x y  = x `mod` y == 0
 
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
