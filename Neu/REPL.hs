module Neu.REPL where

import Neu.FailableIOMonad
import Neu.Evaluator
import Neu.AST
import Data.IORef
import Data.Map

repl environment = do
  putStrLn "Current interpreter state:"
  currentBindings <- readIORef environment
  putStrLn (show (Data.Map.toList currentBindings))
  putStr "neu> "
  expression <- getLine
  if expression /= "quit"
     then do
           executionResult <- failableIO $ (handleInput expression environment)
           case executionResult of
             Right newInterpreterState -> repl newInterpreterState
             Left message -> do { putStrLn ("Error: " ++ (show message))
                                             ; repl environment }
     else
         putStrLn "Bye!!"

handleInput enteredExpression environment = runProgram enteredExpression environment
