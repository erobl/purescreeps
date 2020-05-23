module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Int (decimal, toStringAs)

-- taken from https://github.com/arsdragonfly/purescript-screeps
import Screeps
import Screeps.Game

main :: Effect Unit
main = do
  t <- time
  log $ toStringAs decimal t
