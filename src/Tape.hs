module Tape (
  Value, Tape, tape,
  previousValue, nextValue,
  incrementValue, decrementValue
) where

type Value = Int

type Tape = ([Value], Value, [Value])

tape :: Tape
tape = ([0, 0 ..], 0, [0, 0 ..])

previousValue :: Tape -> Tape
previousValue (leftValue : leftValues, value, rightValues) = (leftValues, leftValue, value : rightValues)
previousValue _ = error "previousValue: this should never happen"

nextValue :: Tape -> Tape
nextValue (leftValues, value, rightValue : rightValues) = (value : leftValues, rightValue, rightValues)
nextValue _ = error "nextValue: this should never happen"

incrementValue :: Tape -> Tape
incrementValue (leftValues, value, rightValues) = (leftValues, (value + 1) `mod` 256, rightValues)

decrementValue :: Tape -> Tape
decrementValue (leftValues, value, rightValues) = (leftValues, (value + 255) `mod` 256, rightValues)
