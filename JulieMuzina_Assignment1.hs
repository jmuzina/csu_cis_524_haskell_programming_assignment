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
    -- Guard clause to handle corner cases first
    | n < 0 = 0
    | n <= 1 = 1
    | otherwise = n * factorial (n - 1)

{-
    #2. Implement a Haskell function isPrime that determines whether a given positive integer n is a prime number or not.
    @param n {Integer} number to check for prime status
    @returns {Bool} whether `n` is prime
-}
isPrime :: Integer -> Bool
isPrime n
    -- Corner cases
    | n <= 1 = False
    | n == 2 = True
    -- Use the built-in `even` function. All even numbers greater than `2` are not prime!
    | even n = False
    -- `n` is an odd number greater than `2`.
    -- `n` is prime if no integers between `3` and the square root of `n` are evenly divisible by `n`
    -- `mod n i == 0` is used as an inline guard for the list comprehension. We only care about numbers that evenly divide by `n`.
    -- So, if the list of numbers divisible by `n` from `3` to `sqrt(n)` is empty, `n` is prime. 
    -- `fromIntegral` is needed to convert `n` to a floating-point value for use in `sqrt`
    | otherwise = null [i | i <- [3..round (sqrt (fromIntegral n))], mod n i == 0]