{- 
    Julie Muzina
    CIS 524
    Haskell Programming Assignment - test cases
    Use `test` to run all tests!
-}

module JulieMuzina_Assignment1_Test where

import JulieMuzina_Assignment1
    ( factorial,
      isPrime,
      fibonacci,
      reverseList,
      removeAllSpaces,
      toLowerCase,
      isPalindrome )

import Debug.Trace
import GHC.Base ( assert )

testFactorial :: IO ()
testFactorial = do
    putStrLn "-----[Factorial tests]-----"
    assert (factorial (-1) == 0) $ putStrLn "Factorial of negative numbers is 0."
    assert (all (\i -> factorial i == i) [0..2]) $ putStrLn "Factorial of all n [0..2] = n"
    assert (factorial 3 == 6) $ putStrLn "factorial(3) == 6"
    assert (factorial 5 == 120) $ putStrLn "factorial(5) == 120"
    assert (factorial 10 == 3628800) $ putStrLn "factorial(10) == 3,628,800"
    putStrLn "-----Factorial tests passed!-----"

testPrime :: IO ()
testPrime = do
    putStrLn "-----[Prime tests]-----"
    assert (not (any isPrime [(-1000)..1])) $ putStrLn "Prime of all n [-1000..1] = False"
    assert (isPrime 2) $ putStrLn "isPrime 2 == True"
    assert (isPrime 3) $ putStrLn "isPrime 3 == True"
    assert (not (isPrime 4)) $ putStrLn "isPrime 4 == False"
    assert (isPrime 5) $ putStrLn "isPrime 5 == True"
    assert (not (isPrime 6)) $ putStrLn "isPrime 6 == False"
    assert (isPrime 97) $ putStrLn "isPrime 97 == True"
    assert (not (isPrime 102)) $ putStrLn "isPrime 102 == False"
    assert (not (isPrime 121)) $ putStrLn "isPrime 121 == False"
    assert (isPrime 7919) $ putStrLn "isPrime 7919 == True"
    assert (not (isPrime 7909)) $ putStrLn "isPrime 7909 == False"
    putStrLn "-----Prime tests passed!-----"

testFibonacci :: IO ()
testFibonacci = do
    putStrLn "-----[Fibonacci tests]-----"
    assert (all (\i -> fibonacci i == 0) [(-1000)..0]) $ putStrLn "Fibonacci of all n [-1000..0] = 0"
    assert (all (\i -> fibonacci i == 1) [1..2]) $ putStrLn "Fibonacci of all n [1..2] = 1"
    assert (fibonacci 3 == 2) $ putStrLn "fibonacci(3) == 2"
    assert (fibonacci 4 == 3) $ putStrLn "fibonacci(4) == 3"
    assert (fibonacci 9 == 34) $ putStrLn "fibonacci(9) == 34"
    assert (fibonacci 15 == 610) $ putStrLn "fibonacci(15) == 618"
    assert (fibonacci 18 == 2584) $ putStrLn "fibonacci(18) == 2584"
    assert (fibonacci 30 == 832040) $ putStrLn "fibonacci(30) == 832,040"
    assert (fibonacci 33 == 3524578) $ putStrLn "fibonacci(33) == 3,524,578"
    putStrLn "-----Fibonacci tests passed!-----"

testReverseList :: IO ()
testReverseList = do
    putStrLn "-----[Reverse List tests]-----"
    assert (null (reverseList [])) $ putStrLn "reverseList [] == []"
    assert (reverseList "" == "") $ putStrLn "reverseList \"\" == \"\""
    assert (reverseList "a" == "a") $ putStrLn "reverseList \"a\" == \"a\""
    assert (reverseList [1] == [1]) $ putStrLn "reverseList [1] == [1]"
    assert (reverseList [1, 2] == [2, 1]) $ putStrLn "reverseList [1, 2] == [2, 1]"
    assert (reverseList "test" == "tset") $ putStrLn "reverseList \"test\" == \"tset\""
    assert (reverseList [5, 2, 6, 8, 2, 1, 6] == [6, 1, 2, 8, 6, 2, 5]) $ putStrLn "reverseList [5, 2, 6, 8, 2, 1, 6] == [6, 1, 2, 8, 6, 2, 5]"
    assert (reverseList "The quick brown fox jumped over the lazy dog" == "god yzal eht revo depmuj xof nworb kciuq ehT") $ putStrLn "reverseList \"The quick brown fox jumped over the lazy dog\" == \"god yzal eht revo depmuj xof nworb kciuq ehT\""
    assert (reverseList [6, 1, 681, 201, 23, 66, 32, 867, 123, 76, -1, -5, 1, 8, 3] == [3, 8, 1, -5, -1, 76, 123, 867, 32, 66, 23, 201, 681, 1, 6]) $ putStrLn "reverseList [6, 1, 681, 201, 23, 66, 32, 867, 123, 76, -1, -5, 1, 8, 3] == [3, 8, 1, -5, -1, 76, 123, 867, 32, 66, 23, 201, 681, 1, 6]"
    assert (reverseList "Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!" == "!erofeb enog sah nam on erehw og yldlob ot ;snoitazilivic wen dna efil wen tuo kees ot ;sdlrow wen egnarts erolpxe ot :noissim raey-evif stI .esirpretnE pihsrats eht fo segayov eht era esehT .reitnorf lanif eht :ecapS") $ putStrLn "reverseList \"Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!\" == \"!erofeb enog sah nam on erehw og yldlob ot ;snoitazilivic wen dna efil wen tuo kees ot ;sdlrow wen egnarts erolpxe ot :noissim raey-evif stI .esirpretnE pihsrats eht fo segayov eht era esehT .reitnorf lanif eht :ecapS\""
    putStrLn "-----Reverse List tests passed!-----"

testRemoveAllSpaces :: IO ()
testRemoveAllSpaces = do
    putStrLn "-----[Remove spaces tests]-----"
    assert (removeAllSpaces "" == "") $ putStrLn "removeAllSpaces \"\" == \"\""
    assert (removeAllSpaces "hello world" == "helloworld") $ putStrLn "removeAllSpaces \"hello world\" == \"helloworld\""
    assert (removeAllSpaces "helloworld" == "helloworld") $ putStrLn "removeAllSpaces \"helloworld\" == \"helloworld\""
    assert (removeAllSpaces "The birch canoe slid on the smooth planks." == "Thebirchcanoeslidonthesmoothplanks.") $ putStrLn "removeAllSpaces \"The birch canoe slid on the smooth planks.\" == \"Thebirchcanoeslidonthesmoothplanks.\""
    assert (removeAllSpaces "Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!" == "Space:thefinalfrontier.ThesearethevoyagesofthestarshipEnterprise.Itsfive-yearmission:toexplorestrangenewworlds;toseekoutnewlifeandnewcivilizations;toboldlygowherenomanhasgonebefore!") $ putStrLn "removeAllSpaces \"Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!\" == \"Space:thefinalfrontier.ThesearethevoyagesofthestarshipEnterprise.Itsfive-yearmission:toexplorestrangenewworlds;toseekoutnewlifeandnewcivilizations;toboldlygowherenomanhasgonebefore!\""
    putStrLn "-----Remove spaces tests passed!-----"

testToLowerCase :: IO ()
testToLowerCase = do
    putStrLn "-----[To Lowercase tests]-----"
    assert (toLowerCase "" == "") $ putStrLn "toLowerCase \"\" == \"\""
    assert (toLowerCase "hello world" == "hello world") $ putStrLn "toLowerCase \"hello world\" == \"hello world\""
    assert (toLowerCase "HeLlo WoRlD" == "hello world") $ putStrLn "toLowerCase \"HeLlo WoRlD\" == \"hello world\""
    assert (toLowerCase "The birch canoe slid on the smooth planks." == "the birch canoe slid on the smooth planks.") $ putStrLn "toLowerCase \"The birch canoe slid on the smooth planks.\" == \"the birch canoe slid on the smooth planks.\""
    assert (toLowerCase "Sit on a potato pan, Otis" == toLowerCase "sIT oN A PoTATo PaN, oTIS") $ putStrLn "toLowerCase \"Sit on a potato pan, Otis\" == toLowerCase \"sIT oN A PoTATo PaN, oTIS\""
    assert (toLowerCase "Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!" == "space: the final frontier. these are the voyages of the starship enterprise. its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!") $ putStrLn "toLowerCase \"Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!\" == \"space: the final frontier. these are the voyages of the starship enterprise. its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!\""
    putStrLn "-----To Lowercase tests passed!-----"

testIsPalindrome :: IO ()
testIsPalindrome = do
    putStrLn "-----[Is Palindrome tests]-----"
    assert (isPalindrome "") $ putStrLn "isPalindrome \"\" == True"
    assert (not(isPalindrome "hello world")) $ putStrLn "isPalindrome \"hello world\" == False"
    assert (isPalindrome "A man a plan a canal Panama") $ putStrLn "isPalindrome \"A man a plan a canal Panama\" == True"
    assert (not(isPalindrome "Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!")) $ putStrLn "isPalindrome \"Space: the final frontier. These are the voyages of the starship Enterprise. Its five-year mission: to explore strange new worlds; to seek out new life and new civilizations; to boldly go where no man has gone before!\" == False"
    assert (isPalindrome "Was it a cat I saw") $ putStrLn "isPalindrome \"Was it a cat I saw\" == True"
    assert (isPalindrome "Sit on a potato pan, Otis" == isPalindrome "sIT oN A PoTATo PaN, oTIS") $ putStrLn "isPalindrome \"Sit on a potato pan, Otis\" == isPalindrome \"sIT oN A PoTATo PaN, oTIS\""
    putStrLn "-----Is Palindrome tests passed!-----"

test :: IO()
test = do
    testFactorial
    testPrime
    testFibonacci
    testReverseList
    testRemoveAllSpaces
    testToLowerCase
    testIsPalindrome