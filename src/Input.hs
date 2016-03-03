module Input (
  Input, read
) where

import Prelude hiding (read)

type Input = String

read :: Input -> Maybe (Char, Input)
read [] = Nothing
read (c : cs) = Just (c, cs)
