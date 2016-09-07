{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

-- Tail Call Recursion Helper
toRevDigitsRec :: Integer -> [Integer] -> [Integer]
toRevDigitsRec n l
  | n <= 0    = l
  | otherwise = toRevDigitsRec (dropLastDigit n) ((lastDigit n) : l)

toRevDigits :: Integer -> [Integer]
toRevDigits n =
  reverse (toRevDigitsRec n [])

-- Exercise 3 -----------------------------------------

-- Tail Call Recursion Helper
doubleEveryOtherRec :: [Integer] -> [Integer] -> [Integer]
doubleEveryOtherRec [] l2               = l2
doubleEveryOtherRec [x] l2              = doubleEveryOtherRec [] (x : l2)
doubleEveryOtherRec (x1 : (x2 : xs)) l2 = doubleEveryOtherRec xs ((2  *x2) : (x1 : l2))

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l =
  reverse (doubleEveryOtherRec l [])

-- Exercise 4 -----------------------------------------

-- Tail Call Recursion Helper
sumDigitsOne :: [Integer] -> Integer -> Integer
sumDigitsOne [] n       = n
sumDigitsOne (x : xs) n = sumDigitsOne xs (x + n)

-- Tail Call Recursion Helper
sumDigitsRec :: [Integer] -> Integer -> Integer
sumDigitsRec [] n = n
sumDigitsRec (x : xs) n = sumDigitsRec xs ((sumDigitsOne (toRevDigits x) 0) + n)

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits l = sumDigitsRec l 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (sumDigits (doubleEveryOther (toRevDigits n))) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

type Info = (Integer, Peg, Peg, Peg)

-- Tail Call Recursion Helper
hanoiRec :: Integer -> Peg -> Peg -> Peg -> [Info] -> [Move] -> [Move]
hanoiRec 1 p1 p2 _ [] s = (p1, p2) : s
hanoiRec 1 p1 p2 _ ((n, f, t, v):is) s = hanoiRec n f t v is ((p1, p2) : s)
hanoiRec n p1 p2 p3 is s = hanoiRec (n - 1) p1 p3 p2 ((1, p1, p2, p3) : ((n - 1, p3, p2, p1) : is)) s

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
  | n <= 0    = []
  | otherwise = reverse (hanoiRec n p1 p2 p3 [] [])
