module Main where

import Text.Parsec



-- ###############
-- # Data types: #
-- ###############

-- The 'Atom' for this program is the "cons-cell" for common lisp.
data Atom = Ident String | Block String [Atom]
  deriving Show



-- #################
-- # IO functions: #
-- #################

-- MAIN, where it all starts.
main = do
  let filename = "testLisp.txt"
  result <- parseFromFile entireFile filename
  case result of
    Right v -> putStrLn ("success: " ++ show v)
    Left err -> do
      fileString <- readFile filename
      putStrLn (
        "whoops, error: "
        ++ show err
        ++ "\n"
        ++ (lines fileString) !! (getErrorRowPos err - 1)
        ++ "\n"
        ++ getRowArrow err)

-- Run a parser over the given file.
parseFromFile p fname = do
  input <- readFile fname
  return (runParser p () fname input)



-- #####################
-- # Parsec functions: #
-- #####################

-- Parse entire file up until the end of file (EOF)
-- with optional buffering whitespace on beginning and end.
entireFile = spaces *> parseAtom <* spaces <* eof

-- Parse an atom that can be either an identity, or a block.
parseAtom = choice [
  (try (between
    (char '(')
    (char ')')
    (Block <$> parseIdent
             -- Parse either 1+ spaces, or and end paren ')'.
             <* (try (many1 space) <|> lookAhead (many1 (char ')')))
           <*> sepBy parseAtom spaces))),
  Ident <$> parseIdent]

-- Parses a single identifier consisting of letters, numbers,
-- and some special characters.
parseIdent = many1 (choice [letter, digit, oneOf "!@#$%^&*+."]
  <?> "letter, digit, symbol")



-- #############################
-- # Error handling functions: #
-- #############################

getRowArrow :: ParseError -> String
getRowArrow err = replicate (getErrorColPos err - 1) '-' ++ "^"

getErrorRowPos :: ParseError -> Int
getErrorRowPos err = (sourceLine . errorPos $ err)

getErrorColPos :: ParseError -> Int
getErrorColPos err = (sourceColumn . errorPos $ err)
