module Output (
  Output,
  write
) where

type Output = String

write :: Char -> Output -> Output
write char output = output ++ [char]
