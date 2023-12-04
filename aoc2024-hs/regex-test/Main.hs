module Main where

import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts(failures),
      Test(TestList, TestCase, TestLabel) )
import System.Exit ( exitFailure, exitSuccess )
import HsRegex (Pattern(..), isMatch)

assertTrue = assertEqual "should be True" True
assertFalse = assertEqual "should be False" False

sanityTest :: Test
sanityTest = TestCase (assertEqual "should be 3" 3 (1 + 2))

pChar0 = Char 'a'
pString0 = String "abba"
pSeq0 = Seq [Char 'a', Char 'b']
pSum0 = Sum [Char 'a', Char 'b']
pNested0 = Sum [Seq [String "ab", Sum [Char 'a', Char 'b']], Char 'c', Char 'd']
pStar0 = Seq [Star (String "ab"), Char 'a']
pPlus0 = Plus (String "ab")
pOpt0 = Opt (String "ab")
pWithAlphaNumChars0 = Seq [String "123", WordChar, Star DigitChar, AlphaChar]

allTests :: Test
allTests = TestList
    [
        -- Char
        TestLabel "Char0 0" (TestCase (assertTrue  (isMatch pChar0 "a"))),
        TestLabel "Char0 1" (TestCase (assertFalse (isMatch pChar0 "b"))),
        TestLabel "Char0 2" (TestCase (assertFalse (isMatch pChar0 " "))),

        -- String
        TestLabel "String0 0" (TestCase (assertFalse (isMatch pString0 ""))),
        TestLabel "String0 1" (TestCase (assertFalse (isMatch pString0 "a"))),
        TestLabel "String0 2" (TestCase (assertFalse (isMatch pString0 "ab"))),
        TestLabel "String0 3" (TestCase (assertTrue  (isMatch pString0 "abba"))),
        TestLabel "String0 4" (TestCase (assertFalse (isMatch pString0 "aba"))),

        -- Seq
        TestLabel "Seq0 0" (TestCase (assertTrue  (isMatch pSeq0 "ab"))),
        TestLabel "Seq0 1" (TestCase (assertFalse (isMatch pSeq0 "ba"))),
        TestLabel "Seq0 2" (TestCase (assertFalse (isMatch pSeq0 "aa"))),
        TestLabel "Seq0 3" (TestCase (assertFalse (isMatch pSeq0 "bb"))),
        TestLabel "Seq0 4" (TestCase (assertFalse (isMatch pSeq0 "aab"))),
        TestLabel "Seq0 5" (TestCase (assertFalse (isMatch pSeq0 "aabb"))),
        TestLabel "Seq0 6" (TestCase (assertFalse (isMatch pSeq0 "abb"))),
        TestLabel "Seq0 7" (TestCase (assertFalse (isMatch pSeq0 ""))),
        TestLabel "Seq0 8" (TestCase (assertFalse (isMatch pSeq0 "aba"))),

        -- Sum
        TestLabel "Sum0 0" (TestCase (assertFalse (isMatch pSum0 "ab"))),
        TestLabel "Sum0 1" (TestCase (assertTrue  (isMatch pSum0 "a"))),
        TestLabel "Sum0 2" (TestCase (assertTrue  (isMatch pSum0 "b"))),
        TestLabel "Sum0 3" (TestCase (assertFalse (isMatch pSum0 "ba"))),
        TestLabel "Sum0 4" (TestCase (assertFalse (isMatch pSum0 "aa"))),
        TestLabel "Sum0 5" (TestCase (assertFalse (isMatch pSum0 ""))),
        TestLabel "Sum0 6" (TestCase (assertFalse (isMatch pSum0 "bb"))),

        -- Nested
        TestLabel "Nested0 0"  (TestCase (assertFalse (isMatch pNested0 ""))),
        TestLabel "Nested0 1"  (TestCase (assertFalse (isMatch pNested0 "a"))),
        TestLabel "Nested0 2"  (TestCase (assertFalse (isMatch pNested0 "b"))),
        TestLabel "Nested0 3"  (TestCase (assertTrue  (isMatch pNested0 "c"))),
        TestLabel "Nested0 4"  (TestCase (assertTrue  (isMatch pNested0 "d"))),
        TestLabel "Nested0 5"  (TestCase (assertFalse (isMatch pNested0 "e"))),
        TestLabel "Nested0 6"  (TestCase (assertFalse (isMatch pNested0 "ab"))),
        TestLabel "Nested0 7"  (TestCase (assertFalse (isMatch pNested0 "ac"))),
        TestLabel "Nested0 8"  (TestCase (assertFalse (isMatch pNested0 "ba"))),
        TestLabel "Nested0 9"  (TestCase (assertTrue  (isMatch pNested0 "aba"))),
        TestLabel "Nested0 10" (TestCase (assertFalse (isMatch pNested0 "abba"))),
        TestLabel "Nested0 11" (TestCase (assertTrue  (isMatch pNested0 "abb"))),

        -- Star
        TestLabel "Star0 0" (TestCase (assertFalse (isMatch pStar0 ""))),
        TestLabel "Star0 1" (TestCase (assertTrue  (isMatch pStar0 "a"))),
        TestLabel "Star0 2" (TestCase (assertFalse (isMatch pStar0 "ab"))),
        TestLabel "Star0 3" (TestCase (assertTrue  (isMatch pStar0 "aba"))),
        TestLabel "Star0 4" (TestCase (assertTrue  (isMatch pStar0 "ababa"))),
        TestLabel "Star0 5" (TestCase (assertFalse (isMatch pStar0 "abbaa"))),

        -- Plus
        TestLabel "Plus0 0" (TestCase (assertFalse (isMatch pPlus0 ""))),
        TestLabel "Plus0 1" (TestCase (assertTrue  (isMatch pPlus0 "ab"))),
        TestLabel "Plus0 2" (TestCase (assertTrue  (isMatch pPlus0 "abab"))),
        TestLabel "Plus0 3" (TestCase (assertTrue  (isMatch pPlus0 "ababab"))),
        TestLabel "Plus0 4" (TestCase (assertFalse (isMatch pPlus0 "a"))),

        -- Opt
        TestLabel "Opt0 0" (TestCase (assertTrue  (isMatch pOpt0 ""))),
        TestLabel "Opt0 1" (TestCase (assertTrue  (isMatch pOpt0 "ab"))),
        TestLabel "Opt0 2" (TestCase (assertFalse (isMatch pOpt0 "abab"))),
        TestLabel "Opt0 3" (TestCase (assertFalse (isMatch pOpt0 "ababab"))),
        TestLabel "Opt0 4" (TestCase (assertFalse (isMatch pOpt0 "a"))),

        -- With alpha-num chars
        TestLabel "WithAlphaNumChars0 0" (TestCase (assertFalse (isMatch pWithAlphaNumChars0 "a"))),
        TestLabel "WithAlphaNumChars0 1" (TestCase (assertFalse (isMatch pWithAlphaNumChars0 "123"))),
        TestLabel "WithAlphaNumChars0 2" (TestCase (assertFalse (isMatch pWithAlphaNumChars0 "123_"))),
        TestLabel "WithAlphaNumChars0 3" (TestCase (assertTrue  (isMatch pWithAlphaNumChars0 "123_a"))),
        TestLabel "WithAlphaNumChars0 4" (TestCase (assertTrue  (isMatch pWithAlphaNumChars0 "123_1a"))),
        TestLabel "WithAlphaNumChars0 5" (TestCase (assertTrue  (isMatch pWithAlphaNumChars0 "123_12a"))),
        TestLabel "WithAlphaNumChars0 6" (TestCase (assertTrue  (isMatch pWithAlphaNumChars0 "123_123a"))),
        TestLabel "WithAlphaNumChars0 7" (TestCase (assertTrue  (isMatch pWithAlphaNumChars0 "123a123a"))),
        TestLabel "WithAlphaNumChars0 8" (TestCase (assertTrue  (isMatch pWithAlphaNumChars0 "123b123a"))),
        TestLabel "WithAlphaNumChars0 9" (TestCase (assertFalse (isMatch pWithAlphaNumChars0 "123b123"))),

        -- Sanity test
        TestLabel "Sanity" sanityTest
    ]

main :: IO ()
main = do
    result <- runTestTT allTests
    if failures result > 0 then exitFailure else exitSuccess
