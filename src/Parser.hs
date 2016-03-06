module Parser (
  parse
) where

import Prelude hiding (Left, Right)

import Data.Maybe (isJust, fromJust)

import Code (Statement(..), Code)

parseStatement :: Char -> Maybe Statement
parseStatement c = case c of
  '<' -> Just Previous
  '>' -> Just Next
  '+' -> Just Increment
  '-' -> Just Decrement
  '.' -> Just Show
  ',' -> Just Read
  '[' -> Just Open
  ']' -> Just Close
  _ -> Nothing

parseStatements :: String -> (Maybe Statement, [Statement])
parseStatements = uncons . map fromJust . filter isJust . map parseStatement
  where
    uncons :: [a] -> (Maybe a, [a])
    uncons [] = (Nothing, [])
    uncons (x : xs) = (Just x, xs)

parse :: String -> Code
parse source = ([], statement, nextStatements)
  where
    (statement, nextStatements) = parseStatements source
