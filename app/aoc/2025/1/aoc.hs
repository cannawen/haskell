{-# LANGUAGE OverloadedStrings #-}

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, (<|>), eof)
import Text.Megaparsec.Char (char, eol)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Turn = TL | TR deriving (Show, Eq)
type Instr = (Turn, Int)

turnP :: Parser Turn
turnP = (char 'L' >> return TL) <|> (char 'R' >> return TR)

instrP :: Parser Instr
instrP = do
        t <- turnP
        n <- L.decimal
        return (t, n)

fileP :: Parser [Instr]
fileP = instrP `sepByLines` eof

sepByLines :: Parser a -> Parser b -> Parser [a]
sepByLines p end = go
    where
        go = do
            atEnd <- (end >> return True) <|> return False
            if atEnd
                then return []
                else do
                    x <- p
                    _ <- (eol >> return ()) <|> return ()
                    xs <- go
                    return (x:xs)

main :: IO ()
main = do
        contents <- readFile "app/aoc/2025/1/input.txt"
        case parse fileP "input" contents of
                Left err -> putStrLn (errorBundlePretty err)
                Right instrs -> do
                        putStrLn "Parsed instructions (first 50):"
                        print (take 50 instrs)

-- I'll go through the file top-to-bottom. For each source line (or small group of related lines) I show the code then a short, slow explanation of exactly what it does.

-- {-# LANGUAGE OverloadedStrings #-}

-- Enables the OverloadedStrings GHC extension so string literals can act like different string-like types. Here it isn't strictly necessary, but it's a common convenience.
-- import Data.Void (Void)

-- Imports the Void type from Data.Void. Void is used as the error/custom state type parameter for the parser type below; it indicates there are no custom error values.
-- import Text.Megaparsec (Parsec, parse, errorBundlePretty, (<|>), eof)

-- Imports key definitions from Megaparsec:
-- Parsec is the parser type constructor used to build parsers.
-- parse runs a parser on input text.
-- errorBundlePretty formats parse errors for printing.
-- (<|>) is the choice operator (try left parser, otherwise right).
-- eof is a parser that succeeds only at end-of-file.
-- import Text.Megaparsec.Char (char, eol)

-- Imports character-level helpers:
-- char parses a single specific character.
-- eol parses an end-of-line (newline).
-- import qualified Text.Megaparsec.Char.Lexer as L

-- Imports the Megaparsec lexer helpers qualified as L. This lets the code call L.decimal for parsing decimal integers.
-- type Parser = Parsec Void String

-- Defines a type alias Parser equal to Parsec Void String.
-- Parsec Void String is a parser that uses String as the input stream and Void as the custom error component. This alias simplifies other type signatures.
-- data Turn = TL | TR deriving (Show, Eq)

-- Declares a small ADT Turn with two constructors TL (turn left) and TR (turn right).
-- deriving (Show, Eq) automatically provides string-printing and equality checks for Turn.
-- type Instr = (Turn, Int)

-- Defines Instr as a type alias for a pair (Turn, Int) — a turn combined with a distance/number.
-- turnP :: Parser Turn

-- Type signature: turnP is a Parser that produces a Turn.
-- turnP = (char 'L' >> return TL) <|> (char 'R' >> return TR)

-- Implementation of turnP:
-- char 'L' parses the character L. >> return TL means: if L parsed, ignore the parsed char and produce TL.
-- <|> tries the left branch; if it fails, it tries the right branch which parses R and returns TR.
-- So turnP yields TL when it sees L, or TR when it sees R.
-- instrP :: Parser Instr

-- Type signature: instrP parses an Instr (a (Turn, Int) pair).
-- instrP = do

--         t <- turnP

--         n <- L.decimal

--         return (t, n)

-- instrP in do-notation:
-- First t <- turnP runs turnP to parse the L or R and binds the resulting Turn to t.
-- Next n <- L.decimal uses Megaparsec's decimal parser to parse one or more digits into an Int, binding that to n.
-- return (t, n) constructs the pair (Turn, Int) and yields it.
-- So an instruction like L10 or R2 becomes (TL, 10) or (TR, 2).
-- fileP :: Parser [Instr]

-- Type signature: fileP parses a list of Instr values (the whole file).
-- fileP = instrP sepByLines eof

-- fileP uses a helper sepByLines with instrP as the element parser and eof as the end condition.
-- It parses zero or more instrP values separated by optional newlines, stopping when eof succeeds.
-- sepByLines :: Parser a -> Parser b -> Parser [a]

-- Type signature: sepByLines takes two parsers — an element parser p and an end parser — and returns a parser that yields a list of a.
-- sepByLines p end = go

--     where

--         go = do

--             atEnd <- (end >> return True) <|> return False

--             if atEnd

--                 then return []

--                 else do

--                     x <- p

--                     _ <- (eol >> return ()) <|> return ()

--                     xs <- go

--                     return (x:xs)

-- This defines a recursive parser go that reads lines until end succeeds.
-- Step-by-step:
-- atEnd <- (end >> return True) <|> return False:
-- Try running end (here eof). If end succeeds, produce True.
-- If end fails, the <|> return False branch runs and produces False.
-- This is a way to peek for end-of-input without consuming input irreversibly.
-- if atEnd then return []:
-- If we are at the end, return the empty list (no more items).
-- else do x <- p:
-- Otherwise, parse one element x using the element parser p (instrP).
-- _ <- (eol >> return ()) <|> return ():
-- Then try to consume an end-of-line if present. If there's an eol (newline), consume it; otherwise do nothing.
-- This allows both newline-separated lines and the final line possibly not ending with a newline.
-- xs <- go recursively parse the rest of the list.
-- return (x:xs) constructs the result list with x as head.
-- In short: sepByLines repeatedly parses p until end is reached, allowing optional newlines between items.
-- main :: IO ()

-- The program entry point.
-- main = do

--         contents <- readFile "app/aoc/2025/1/input.txt"

-- Reads the whole input file into contents as a String. The path is relative to the project root.
--         case parse fileP "input" contents of

-- Runs the Megaparsec parse function with:
-- fileP as the parser,
-- "input" as the source name used in error messages,
-- contents as the input string.
-- parse returns either Left err (parse failure) or Right instrs (success).
--                 Left err -> putStrLn (errorBundlePretty err)

-- If parsing fails, pretty-print the parse error bundle to the console.
--                 Right instrs -> do

--                         putStrLn "Parsed instructions (first 50):"

--                         print (take 50 instrs)

-- If parsing succeeds, bind the parsed list to instrs, then:
-- Print a header line.
-- Print the first 50 parsed instructions for inspection.
-- Summary of how parsing works end-to-end:

-- main reads the input file into a String.
-- parse fileP "input" contents runs fileP against that String.
-- fileP repeatedly runs instrP (via sepByLines) until eof.
-- Each instrP parses a Turn with turnP (matching L or R) then a decimal number with L.decimal.
-- sepByLines allows each instruction to be followed by an optional newline; it stops when eof indicates no more input.
-- On success, you get a [Instr] where each Instr is (TL|TR, Int), e.g. (TL,10).