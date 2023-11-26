{- 
    Julie Muzina
    CIS 524
    Haskell Programming Assignment
-}

module JulieMuzina_Assignment1 where

-- Import needed for the `toLowerCase` function
import Data.Char (toLower)

{-
    #1. Write a Haskell function factorial that calculates the factorial of a given integer n. 
    Ensure the function handles edge cases like 0 and negative numbers.
    @param {Integer} n - number to calculate factorial for
    @returns {Integer} n!
-}
factorial :: Integer -> Integer
factorial n
    -- Guard clause to handle corner cases first
    | n < 0 = 0
    | n <= 2 = n
    | otherwise = n * factorial (n - 1)

{-
    #2. Implement a Haskell function isPrime that determines whether a given positive integer n is a prime number or not.
    @param {Integer} n - number to check for prime status
    @returns {Bool} whether `n` is prime
-}
isPrime :: Integer -> Bool
isPrime n
    -- Corner cases
    | n <= 1 = False
    | n == 2 = True
    -- Use the built-in `even` function. All even numbers greater than `2` are not prime!
    | even n = False
    {-
        `n` is an odd number greater than `2`.
        `n` is prime if no integers between `3` and the square root of `n` are evenly divisible by `n`
        `mod n i == 0` is used as an inline guard for the list comprehension. We only care about numbers that evenly divide by `n`.
        So, if the list of numbers divisible by `n` from `3` to `sqrt(n)` is empty, `n` is prime. 
        `fromIntegral` is needed to convert `n` to a floating-point value for use in `sqrt`
    -}
    | otherwise = null [i | i <- [3..round (sqrt (fromIntegral n))], mod n i == 0]

{-
    #3. Create a Haskell function fibonacci that generates the nth Fibonacci number using a recursive approach. 
    Make sure to handle edge cases, such as when n is 0 or 1.
    @param {Integer} n - Position of the desired fibonacci number within the fibonacci sequence
    @returns {Integer} `n`th fibonacci number
-}
fibonacci :: Integer -> Integer
fibonacci n
    -- Corner / base cases
    | n < 0 = 0
    | n <= 1 = n
    -- Recursive step
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

{-
    #4. Write a Haskell function reverseList that reverses a list (or a string) using recursion.
    For example, reverseList [1, 2, 3] should return [3, 2, 1]
    @param {Foldable} l - List or string to reverse
    @returns {Foldable} `l` in reverse order
-}
reverseList :: [t] -> [t]
reverseList l
    -- Empty lists and lists of length 1 are already reversed
    | null l = []
    | length l == 1 = l
    {-- List is not empty, reverse it.
        Take the last character of `l` and prepend (:) it to the result of a recursive call with all characters of `l` except the last one (init)
        reverseList "test" -> "t" : reverseList "tes" -> "tset"
            reverseList "tes" -> "s" : reverseList "te" -> "set"
                reverseList "te" -> "e" : reverseList "t" -> "et"
                    reverseList "t" -> "t"
    --}
    | otherwise = last l : reverseList (init l)

{-
    #5. Implement a Haskell function isPalindrome that checks if a given string is a
    palindrome (reads the same forwards and backward), ignoring spaces and case
    sensitivity. For example, "A man a plan a canal Panama" should be considered a
    palindrome.
-}

{-
    Remove all spaces from a string
    @param {String} - string to remove spaces from
    @returns {String} - input string without spaces
-}
removeAllSpaces :: String -> String
removeAllSpaces = filter (/= ' ')

{-
    Convert a string to lowercase
    @param {String} - string to convert to lowercase
    @returns {String} - input string converted to lowercase
-}
toLowerCase :: String -> String
toLowerCase = map toLower

{-
    Check whether a string is a palindrome
    @param {String} s - String to check palindrome state
    @returns {Bool} whether `s` is a palindrome,, ignoring spaces and case.
-}
isPalindrome :: String -> Bool
{-
    Luckily we already have a function to reverse a string from #4!
    A string is a palindrome if its reverse is equal to itself.
    We also use the helper functions `toLowerCase` and `removeAllSpaces` to make the palindrome check ignore case and spaces.
-}
isPalindrome s = removeAllSpaces (toLowerCase s) == reverseList (removeAllSpaces (toLowerCase s))
