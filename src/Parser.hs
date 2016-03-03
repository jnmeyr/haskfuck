module Parser (
  parse
) where

import Prelude hiding (Left, Right)

import qualified Data.Maybe as Maybe (isJust, mapMaybe, listToMaybe)

import Types (Source, Statement(..), Code)

parseStatement :: Char -> Maybe Statement
parseStatement c = case c of
  '<' -> Just Left
  '>' -> Just Right
  '+' -> Just Increment
  '-' -> Just Decrement
  '.' -> Just Show
  ',' -> Just Read
  '[' -> Just Open
  ']' -> Just Close
  _ -> Nothing

parse :: Source -> Code
parse source = ([], statement, statements')
  where
    statements = Maybe.mapMaybe parseStatement source
    statement = Maybe.listToMaybe statements
    statements' = if Maybe.isJust statement then tail statements else []
