{- 
    Julie Muzina
    CIS 524
    Haskell Programming Assignment
-}

{-
    #1. Write a Haskell function factorial that calculates the factorial of a given integer n. 
    Ensure the function handles edge cases like 0 and negative numbers.
    @param n {Integer} number to calculate factorial for
    @returns {Integer} n!
-}
factorial :: Integer -> Integer
factorial n
    -- Guard clause to handle edge cases first
    | n < 0 = 0
    | n <= 1 = 1
    | otherwise = n * factorial (n - 1)