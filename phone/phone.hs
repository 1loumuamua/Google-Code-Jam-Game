module Main
where

import Text.ParserCombinators.Parsec
import System.IO hiding (try)
import System.Environment
-- PROBLEM-SPECIFIC IMPORTS --
import Data.List (sort)
import Control.Monad
import Control.Arrow
import Data.List

------------------------------

-- the input file will be parsed into a list of some type representing
-- individual cases to be solved by our algorithm:
type Input = [Case]

-- our algorithms will produce a list of some type SolutionCase:
type Solution = [SolvedCase]

-- we convert each solved case into a String, which is zipped with
-- the standard "Case #x: " strings for the final output
type Output = [String]

-- --------------- BEGIN EDITING --------------- --

-- DEFINE A TYPE TO REPRESENT A SINGLE UNSOLVED TEST CASE: --
type Case =
    -- EXAMPLE: a pair of lists of Ints (our "vectors"):
    String


-- DEFINE A TYPE TO REPRESENT A SINGLE SOLVED TEST CASE: --
type SolvedCase =
    -- EXAMPLE: our Minimum Scalar Product as an Int:
    String


-- -- -- ALGORITHMS -- -- --
removeDigit :: String -> String -> String
removeDigit "" letters = letters
removeDigit digit letters = removeDigit (tail digit) (delete (head digit) letters)

processNumber :: String -> String -> String
processNumber "" digits = digits
processNumber letters digits
  | elem 'Z' letters = processNumber (removeDigit "ZERO" letters) (digits ++ "0")
  | elem 'W' letters = processNumber (removeDigit "TWO" letters) (digits ++ "2")
  | elem 'U' letters = processNumber (removeDigit "FOUR" letters) (digits ++ "4")
  | elem 'R' letters = processNumber (removeDigit "THREE" letters) (digits ++ "3")
  | elem 'G' letters = processNumber (removeDigit "EIGHT" letters) (digits ++ "8")
  | elem 'F' letters = processNumber (removeDigit "FIVE" letters) (digits ++ "5")
  | elem 'X' letters = processNumber (removeDigit "SIX" letters) (digits ++ "6")
  | elem 'S' letters = processNumber (removeDigit "SEVEN" letters) (digits ++ "7")
  | elem 'O' letters = processNumber (removeDigit "ONE" letters) (digits ++ "1")
  | elem 'N' letters = processNumber (removeDigit "NINE" letters) (digits ++ "9")
  | otherwise = "There some fuckery going on"


-- SOLVE A TEST CASE HERE:
algorithm :: Case -> SolvedCase
algorithm letters =
    -- EXAMPLE: simply sort both lists, reverse one, and combine:
      processNumber letters ""

-- -- -- PARSING INPUT -- -- --


-- DEFINE PARSER FOR A TEST CASE: --
caseParser :: Parser Case
caseParser = do
    -- EXAMPLE: parses a case consisting of 3 lines: the first describes the
    -- number n of elements in the following two lines, the next two lines
    -- have n space-separated elements:
    l <- wholeLine
    return l


-- -- -- FORMAT OUTPUT -- -- --

-- DEFINE A FUNCTION FROM AN INDIVIDUAL SolvedCase -> String.
formatCase :: SolvedCase -> String
formatCase sol =
    -- EXAMPLE: nothing to speak of here:
    sort sol



-- --------------- STOP EDITING --------------- --


--------------------
-- IO BOILERPLATE --
--------------------


main = do
    -- pass the input file name to our program:
    (f:_) <- getArgs
    file <- readFile f

    -- start parsing, solve problem, and prepare outhroatfuckingtput:
    let inp = parse mainParser "errmsg" file
    case inp of
      Left err -> print err

      Right stuff -> do
        let solution = map algorithm stuff
        let solutionStrings = map formatCase solution
        let outp = zipWith (++) prefixes solutionStrings
        putStr $ unlines outp
    -- write the prepared output to screen:


-- dies with error, or returns some datatype with our parsed data:

-- to begin parsing, we read in a line containing the number of test cases
-- to follow. We parse them with caseParser, returning a list:
mainParser :: Parser Input
mainParser = do
    n <- word
    ms <- count (read n) caseParser
    return ms
   <?> "mainParser"

-- strings to prepend to output:
prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]



---------------------
-- VARIOUS PARSERS --
---------------------


-- -- LINE PARSERS -- --


-- a single line String, up to the newline:
wholeLine :: Parser String
wholeLine = manyTill anyChar (try newline) <?> "wholeLine"

-- parse a String with whitespace-separated values into [String]
whiteSepLine = manyTill spaceSepWord newline <?> "whiteSepLine"


-- -- WORD PARSERS -- --

-- a single word followed by whitespace (space, newline, etc.):
word = do
    w <- many1 nonWhite
    spaces
    return w
   <?> "word"

-- a single word followed by one or more ' ' characters (won't consume '\n')
spaceSepWord = do
    w <- many1 nonWhite
    many (char ' ')
    return w
   <?> "spaceSepWord"


-- e.g. "hello:world" ---> ("hello","world")
-- won't consume newlines

twoWordsSepBy c = do
    x <- manyTill nonWhite (try$ char c)
    y <- many1 nonWhite
    many (char ' ')
    return (x,y)
   <?> "twoWordsSepBy"


-- -- CHARACTER PARSERS -- --

-- nonWhitespace character:
nonWhite = noneOf " \v\f\t\r\n" <?> "nonWhite"
