module Interpreter (
  interpret
) where

import Prelude hiding (id, read)

import Control.Category
import Control.Arrow

import Data.Either (isLeft)

import Tape (Tape, tape, previousValue, nextValue, incrementValue, decrementValue)
import Code (Statement(..), Code, isStatement, nextStatement, nextCloseStatement, prevOpenStatement)
import Input (Input, read)
import Output (Output, output, write)

(?>) :: ArrowChoice a => (b -> Bool) -> a b b -> a b (Either b b)
(?>) p f = arr (\ b -> if p b then Left b else Right b) >>> (f +++ returnA)

infixl 2 ?>

(<|>) :: ArrowChoice a => a b (Either b b) -> a b (Either b b) -> a b (Either b b)
(<|>) f g = f &&& g >>> arr (\ (eb, eb') -> if isLeft eb then eb else eb')

infixl 1 <|>

fixpoint :: ArrowChoice a => a b (Either b b) -> a b b
fixpoint f = f >>> (fixpoint f ||| returnA)

type Interpreter = (Code, Input, Tape, Output) -> (Code, Input, Tape, Output)

isInterpreter :: Statement -> (Code, Input, Tape, Output) -> Bool
isInterpreter statement (code, _, _, _) = isStatement statement code

previousInterpreter :: Interpreter
previousInterpreter (code, input, tape, output) =
  (nextStatement code, input, previousValue tape, output)

nextInterpreter :: Interpreter
nextInterpreter (code, input, tape, output) =
  (nextStatement code, input, nextValue tape, output)

incrementInterpreter :: Interpreter
incrementInterpreter (code, input, tape, output) =
  (nextStatement code, input, incrementValue tape, output)

decrementInterpreter :: Interpreter
decrementInterpreter (code, input, tape, output) =
  (nextStatement code, input, decrementValue tape, output)

showInterpreter :: Interpreter
showInterpreter (code, input, tape@(_, value, _), output) =
  (nextStatement code, input, tape, write value output)

readInterpreter :: Interpreter
readInterpreter (code, input, (leftValues, _, rightValues), output) =
  let (value, input') = read input in (nextStatement code, input', (leftValues, value, rightValues), output)

openInterpreter :: Interpreter
openInterpreter (code, input, tape@(_, value, _), output) =
  ((if value == 0 then nextCloseStatement else nextStatement) code, input, tape, output)

closeInterpreter :: Interpreter
closeInterpreter (code, input, tape@(_, value, _), output) =
  ((if value /= 0 then prevOpenStatement else nextStatement) code, input, tape, output)

interpreter :: Interpreter
interpreter = fixpoint $
  isInterpreter Previous  ?> previousInterpreter  <|>
  isInterpreter Next      ?> nextInterpreter      <|>
  isInterpreter Increment ?> incrementInterpreter <|>
  isInterpreter Decrement ?> decrementInterpreter <|>
  isInterpreter Show      ?> showInterpreter      <|>
  isInterpreter Read      ?> readInterpreter      <|>
  isInterpreter Open      ?> openInterpreter      <|>
  isInterpreter Close     ?> closeInterpreter

interpret :: Code -> Input -> Output
interpret code input = case interpreter (code, input, tape, output) of (_, _, _, output) -> output
