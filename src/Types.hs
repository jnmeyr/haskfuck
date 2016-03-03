module Types (
  Source,
  Statement(..),
  Code
) where

type Source = String

data Statement =
  Left |
  Right |
  Increment |
  Decrement |
  Show |
  Read |
  Open |
  Close

type Code = ([Statement], Maybe Statement, [Statement])
