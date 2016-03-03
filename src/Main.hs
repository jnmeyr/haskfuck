module Main (
  main
) where

import Data.Foldable (forM_)

import Types (Source, Statement(..), Code)
import Interpreter (interpret)

demo :: String
demo = "++++++++++\n[\n >+++++++>++++++++++>+++>+<<<<-\n]\n>++.\n>+.\n+++++++.\n.\n+++.\n>++.\n<<+++++++++++++++.\n>.\n+++.\n------.\n--------.\n>+.\n>.\n+++.\n"

main :: IO ()
main = forM_ (interpret demo "") putStrLn
