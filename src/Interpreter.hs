module Interpreter (
  interpret
) where

import Prelude hiding (Left, Right, read)

import qualified Data.Char as Char (chr, ord)

import Types (Source, Statement(..), Code)
import Tape (Tape, tape, moveLeft, moveRight, incrementValue, decrementValue)
import Parser (parse)
import Input (Input, read)
import Output (Output, write)

type State = (Input, Tape, Output)

nextStatement :: Code -> Code
nextStatement (_, Nothing, _) = error "nextStatement: this should never happen"
nextStatement (prevStatements, Just prevStatement, []) = (prevStatement : prevStatements, Nothing, [])
nextStatement (prevStatements, Just prevStatement, nextStatement : nextStatements) = (prevStatement : prevStatements, Just nextStatement, nextStatements)

prevStatement :: Code -> Code
prevStatement (_, Nothing, _) = error "prevStatement: this should never happen"
prevStatement ([], Just nextStatement, nextStatements) = ([], Nothing, nextStatement : nextStatements)
prevStatement (prevStatement : prevStatements, Just nextStatement, nextStatements) = (prevStatements, Just prevStatement, nextStatement : nextStatements)

nextCloseStatement :: Code -> Code
nextCloseStatement = nextCloseStatement' 0 . nextStatement

nextCloseStatement' :: Int -> Code -> Code
nextCloseStatement' 0 code@(_, Just Close, _) = nextStatement code
nextCloseStatement' n code@(_, Just Close, _) = nextCloseStatement' (n - 1) (nextStatement code)
nextCloseStatement' n code@(_, Just Open, _) = nextCloseStatement' (n + 1) (nextStatement code)
nextCloseStatement' n code = nextCloseStatement' n (nextStatement code)

prevOpenStatement :: Code -> Code
prevOpenStatement = prevOpenStatement' 0 . prevStatement

prevOpenStatement' :: Int -> Code -> Code
prevOpenStatement' 0 code@(_, Just Open, _) = nextStatement code
prevOpenStatement' n code@(_, Just Open, _) = prevOpenStatement' (n - 1) (prevStatement code)
prevOpenStatement' n code@(_, Just Close, _) = prevOpenStatement' (n + 1) (prevStatement code)
prevOpenStatement' n code = prevOpenStatement' n (prevStatement code)

executeStatement :: Statement -> Code -> State -> Maybe (Code, State)
executeStatement Left code (input, tape, output) = Just (nextStatement code, (input, moveLeft tape, output))
executeStatement Right code (input, tape, output) = Just (nextStatement code, (input, moveRight tape, output))
executeStatement Increment code (input, tape, output) = Just (nextStatement code, (input, incrementValue tape, output))
executeStatement Decrement code (input, tape, output) = Just (nextStatement code, (input, decrementValue tape, output))
executeStatement Show code (input, tape@(_, value, _), output) = Just (nextStatement code, (input, tape, output'))
  where
    char = Char.chr value
    output' = write char output
executeStatement Read code (input, (lefts, _, rights), output) = codeState
  where
    codeState = case read input of
      Just (char, input') ->
        Just (nextStatement code, (input', (lefts, value, rights), output))
          where
            value = Char.ord char
      Nothing ->
        Nothing
executeStatement Open code state@(_, (_, value, _), _)
  | value == 0 = Just (nextCloseStatement code, state)
  | otherwise = Just (nextStatement code, state)
executeStatement Close code state@(_, (_, value, _), _)
  | value /= 0 = Just (prevOpenStatement code, state)
  | otherwise = Just (nextStatement code, state)

executeCode :: Code -> State -> Maybe Output
executeCode (_, Nothing, _) (_, _, output) = Just output
executeCode code@(_, Just statement, _) state = case executeStatement statement code state of
  Just (code', state') ->
    executeCode code' state'
  Nothing ->
    Nothing

interpret :: Source -> Input -> Maybe Output
interpret source input = executeCode (parse source) (input, tape, "")