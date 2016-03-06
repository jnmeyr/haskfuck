module Main (
  main
) where

import System.Environment (getArgs)

import Parser (parse)
import Interpreter (interpret)

helloWorld :: (String, String)
helloWorld = ("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++.", "")

main :: IO ()
main = do
  args <- getArgs
  let (source, input) | length args == 2 = (args !! 0, args !! 1)
                      | length args == 1 = (args !! 0, "")
                      | otherwise        = helloWorld
  putStrLn $ interpret (parse source) input
