module Neu.Evaluator where
import Neu.FailableIOMonad
import Neu.AST
import Neu.Parser
import Data.Map as Map
import Data.IORef

evaluate :: Expression -> Environment -> FailableIO Value
evaluate (Const a) environment = do return (NumericValue a)
evaluate (StringLiteral a) environment = do return (StringValue a)
evaluate (Addition b c) environment = performBinaryOp b c environment additionOp
evaluate (Subtraction b c) environment = performBinaryOp b c environment subtractionOp
evaluate (Multiplication b c) environment = performBinaryOp b c environment multiplicationOp
evaluate (Division b c) environment = binaryNumericOp b c (/) environment
evaluate (Comparison GreaterThan b c) environment = binaryBooleanOp b c (>) environment
evaluate (Comparison LessThan b c) environment = binaryBooleanOp b c (<) environment
evaluate (Variable id) environment = getVariableValue environment id
evaluate (FunctionCall arguments func) environment = do (Function identifiers body env) <- evaluateFunctionValue func environment
                                                        evaluatedArguments <- mapM (\x -> evaluate x environment) arguments
                                                        closureEnv <- io $ createClosureEnvironment env identifiers evaluatedArguments
                                                        executionResult <- executeParsedStatements body closureEnv
                                                        case executionResult of
                                                          ContinueResult finalState -> do return Null
                                                          ReturnResult retVal finalState -> do return retVal
evaluate (FunctionDefinition arguments body) environment = do return (Function arguments body environment)


createClosureEnvironment env identifiers evaluatedArguments = do outer <- readIORef env
                                                                 newIORef (union (Map.fromList (zip identifiers evaluatedArguments)) outer)

performBinaryOp a b frame operation = do
  term <- evaluate a frame
  operand <- evaluate b frame
  result <- operation term operand
  return result

binaryOpFailureMessage opName arg1 arg2 = "Cannot perform '" ++ opName ++ "' with arguments " ++ (show arg1) ++ " and " ++ (show arg2)

evaluateFunctionValue funcExpr frame = do
  func <- evaluate funcExpr frame
  case func of
    (Function identifiers body env) -> return func
    other -> fail $ "Can't call " ++ show(other)

additionOp term addend  = case (term, addend) of
                            (NumericValue t1, NumericValue t2) -> return (NumericValue (t1 + t2))
                            (StringValue t1, StringValue t2)   -> return (StringValue (t1 ++ t2))
                            (t1, t2) -> fail (binaryOpFailureMessage "add" t1 t2)

subtractionOp term subtrahend = case (term, subtrahend) of
                                  (NumericValue t1, NumericValue t2) -> return (NumericValue (t1 - t2))
                                  (t1, t2) -> fail (binaryOpFailureMessage "subtract" t1 t2)

multiplicationOp term multiple = case (term, multiple) of
                                   (NumericValue t1, NumericValue t2) -> return (NumericValue (t1 * t2))
                                   (t1, t2) -> fail (binaryOpFailureMessage "multiply" t1 t2)

divisionOp term divisor = case (term, divisor) of
                                   (NumericValue t1, NumericValue t2) -> return (NumericValue (t1 / t2))
                                   (t1, t2) -> fail (binaryOpFailureMessage "divide" t1 t2)

binaryNumericOp a b op frame = do
  NumericValue t1 <- evaluate a frame
  NumericValue t2 <- evaluate b frame
  return (NumericValue (op t1 t2))

binaryBooleanOp a b op frame = do
  NumericValue t1 <- evaluate a frame
  NumericValue t2 <- evaluate b frame
  return (BoolValue (op t1 t2))

performAssignment id exp state = do
  rhs <- evaluate exp state
  assignVariableValue state id rhs
  return state

execute :: Statement -> Environment -> FailableIO ExecutionResult
execute (AssignExpression id exp) environment    = do newEnvironment <- (performAssignment id exp environment)
                                                      return (ContinueResult newEnvironment)
execute (Print exp) environment                  = do result <- evaluate exp environment
                                                      io $ putStrLn (show result)
                                                      return (ContinueResult environment)
execute (Load path) environment		   = do content <- io $ readFile path
                                                programResult <- runProgram content environment
                                                return (ContinueResult programResult)

execute (WhileLoop condition statements) environment = do conditionResult <- evaluate condition environment
                                                          if (evaluatesToTrue conditionResult)
                                                            then do ContinueResult bodyResult <- executeParsedStatements statements  environment
                                                                    nextExecutionResult <- (execute (WhileLoop condition statements) bodyResult)
                                                                    return nextExecutionResult
                                                            else do return (ContinueResult environment)

execute (IfStatement condition statements) environment = do conditionResult <- evaluate condition environment
                                                            if (evaluatesToTrue conditionResult)
                                                               then do executionResult <- executeParsedStatements statements environment
                                                                       return executionResult
                                                               else do return (ContinueResult environment)

execute (Return exp) environment                 = do result <- evaluate exp environment
                                                      return (ReturnResult result environment)

execute (ExpressionStatement exp) environment    = do result <- evaluate exp environment
                                                      return (ContinueResult environment)

                                                                                    
evaluatesToTrue (BoolValue val) = val

runProgram ::  String -> Environment -> FailableIO Environment
runProgram  input interpreterState
        = case (parseNeuProgram input) of
            Left err -> throwError $ DefaultError ("Parse error: " ++ (show err))
            Right parsedStatements  -> do ContinueResult finalState <- (executeParsedStatements parsedStatements interpreterState)
                                          return finalState

executeParsedStatements :: [Statement] -> Environment -> FailableIO ExecutionResult
executeParsedStatements [] initialState      = return (ContinueResult initialState)
executeParsedStatements (s1:ss) initialState = do intermediateResult <- execute s1 initialState
                                                  case intermediateResult of
                                                    ReturnResult retVal intermediateState -> do return (ReturnResult retVal intermediateState)
                                                    ContinueResult intermediateState      -> do finalState <- (executeParsedStatements ss intermediateState)
                                                                                                return finalState



getVariableValue :: Environment -> Identifier -> FailableIO Value
getVariableValue environment identifier = do env <- io $ readIORef environment
                                             case (Map.lookup identifier env) of
                                               Just value -> return value
                                               Nothing    -> fail ("Unassigned variable: " ++ identifier)

assignVariableValue :: Environment -> Identifier -> Value -> FailableIO ()
assignVariableValue environment id value = do env <- io $ readIORef environment
                                              io $ writeIORef environment (Map.insert id value env)

