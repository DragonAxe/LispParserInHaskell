module Main where

-- I import qualified so that it's clear which
-- functions are from the parsec library:
-- import qualified Text.Parsec as Parsec
import qualified Text.Parsec as P

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- Either a name/identifier or another lisp block
data Atom = Node String | Cell Lisp
  deriving Show

-- A lisp block containing a function name and list of parameters
data Lisp = Lisp (String, [Atom])
  deriving Show

-- MAIN, where it all happens.
main = do
  let result = P.parse entireFile
                       "(source)"
                       "(print (cat hello hi) (abd) (hi) endprint)"
  case result of
    Right v -> putStrLn ("success: " ++ show v)
    Left err -> putStrLn ("whoops, error: " ++ show err)

-- Parse entire file up until the end of file (EOF)
-- with optional buffering whitespace on beginning and end.
entireFile = parseSeparator *> lisp <* parseSeparator <* P.eof

-- Parse a matching set of parenthesis containing innerLisp.
lisp = Lisp <$> P.between (P.char '(' <?> "[opening paren]")
                          (P.char ')' <?> "[closing paren]")
                          innerLisp

-- Parse text inside of a matching set of parenthesis consisting of an
-- identifier followed by an optional set of parameters.
innerLisp = (,) <$> parseIdentifier
                <*> (parseSeparator
                     >> P.sepBy parseParam (parseSeparator))

-- Parses a single parameter inside a lisp block.
parseParam = Cell <$> (P.try lisp) P.<|> Node <$> (P.try parseIdentifier)
  <?> "[a parameter (one of Param or SubLisp)]"

-- Parses a single name/identifier, (basically an atom, or function name).
parseIdentifier = P.many1 (P.letter <?> "[valid name/identifier]")

-- Parses any kind of whitespace.
parseSeparator = P.spaces <?> "[whitespace]"
