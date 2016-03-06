module Output (
  Output, output,
  write
) where

import Data.Char (chr)

import Tape (Value)

type Output = String

output :: String
output = ""

write :: Value -> Output -> Output
write value output = output ++ [chr value]
