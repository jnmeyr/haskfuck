module Code (
  Statement(..), Code,
  hasStatement, isStatement,
  nextStatement, nextCloseStatement,
  prevStatement, prevOpenStatement
) where

import Data.Maybe (isJust)

data Statement =
  Previous | Next |
  Increment | Decrement |
  Show | Read |
  Open | Close
    deriving Eq

type Code = ([Statement], Maybe Statement, [Statement])

hasStatement :: Code -> Bool
hasStatement (_, mStatement, _) = isJust mStatement

isStatement :: Statement -> Code -> Bool
isStatement _ (_, Nothing, _) = False
isStatement thatStatement (_, Just thisStatement, _) = thisStatement == thatStatement

nextStatement :: Code -> Code
nextStatement (_, Nothing, _) = error "nextStatement: this should never happen"
nextStatement (prevStatements, Just prevStatement, []) = (prevStatement : prevStatements, Nothing, [])
nextStatement (prevStatements, Just prevStatement, nextStatement : nextStatements) = (prevStatement : prevStatements, Just nextStatement, nextStatements)

nextCloseStatement :: Code -> Code
nextCloseStatement = nextCloseStatement' 0 . nextStatement
  where
    nextCloseStatement' :: Int -> Code -> Code
    nextCloseStatement' 0 code@(_, Just Close, _) = nextStatement code
    nextCloseStatement' n code@(_, Just Close, _) = nextCloseStatement' (n - 1) (nextStatement code)
    nextCloseStatement' n code@(_, Just Open, _) = nextCloseStatement' (n + 1) (nextStatement code)
    nextCloseStatement' n code = nextCloseStatement' n (nextStatement code)

prevStatement :: Code -> Code
prevStatement (_, Nothing, _) = error "prevStatement: this should never happen"
prevStatement ([], Just nextStatement, nextStatements) = ([], Nothing, nextStatement : nextStatements)
prevStatement (prevStatement : prevStatements, Just nextStatement, nextStatements) = (prevStatements, Just prevStatement, nextStatement : nextStatements)

prevOpenStatement :: Code -> Code
prevOpenStatement = prevOpenStatement' 0 . prevStatement
  where
    prevOpenStatement' :: Int -> Code -> Code
    prevOpenStatement' 0 code@(_, Just Open, _) = nextStatement code
    prevOpenStatement' n code@(_, Just Open, _) = prevOpenStatement' (n - 1) (prevStatement code)
    prevOpenStatement' n code@(_, Just Close, _) = prevOpenStatement' (n + 1) (prevStatement code)
    prevOpenStatement' n code = prevOpenStatement' n (prevStatement code)
