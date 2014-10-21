module Neu where

import Neu.REPL
import Neu.AST

main = do
  env <- initialEnvironment
  repl env