module Input (
  Input, read
) where

import Prelude hiding (read)

import Data.Char (ord)

import Tape (Value)

type Input = String

read :: Input -> (Value, Input)
read [] = (0, [])
read (character : characters) = (ord character, characters)
