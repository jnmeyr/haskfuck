module Tape (
  Tape, tape,
  moveLeft, moveRight,
  incrementValue, decrementValue
) where

type Value = Int

type Tape = ([Value], Value, [Value])

tape :: Tape
tape = ([0, 0 ..], 0, [0, 0 ..])

moveLeft :: Tape -> Tape
moveLeft (l : lefts, value, rights) = (lefts, l, value : rights)

moveRight :: Tape -> Tape
moveRight (lefts, value, r : rights) = (value : lefts, r, rights)

incrementValue :: Tape -> Tape
incrementValue (lefts, value, rights) = (lefts, value', rights)
  where
    value' = (value + 1) `mod` 256

decrementValue :: Tape -> Tape
decrementValue (lefts, value, rights) = (lefts, value', rights)
  where
    value' = (value + 255) `mod` 256
