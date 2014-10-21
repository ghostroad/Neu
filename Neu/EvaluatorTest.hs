module Neu.EvaluatorTest(evaluatorTests) where
import Test.HUnit
import Neu.Evaluator
import Neu.AST
import Neu.FailableIOMonad
import Data.Map as Map
import Data.IORef

initialBindings = []

getProgramResult program initialBindings = do initialEnvironment <- newEnvironmentFrom initialBindings
                                              programResult <- failableIO $ (runProgram program initialEnvironment)
                                              return programResult

assertVariableBinding :: String -> Map Identifier Value -> (Identifier, Value) -> Assertion
assertVariableBinding description actualBindings expectedBinding = let variableName = fst expectedBinding
                                                                       expectedValue = snd expectedBinding
                                                                       actualValue = (Map.lookup variableName actualBindings) :: Either String Value
                                                                   in do case actualValue of
                                                                           Right val -> assertEqual description val expectedValue
                                                              

assertSuccessfulExecutionResult description program initialBindings expectedResultantBindings = do
  programResult <- getProgramResult program initialBindings
  case programResult of
    Right finalState -> do finalStateMapping <- readIORef finalState
                           sequence_ $ Prelude.map (assertVariableBinding description finalStateMapping) expectedResultantBindings
    Left err         -> fail $ "Program failed: " ++ (show err)

assertFailingExecutionResult description program initialBindings expectedError = do
  programResult <- getProgramResult program initialBindings
  case programResult of
    Right finalState -> fail $ "Program succeeded but expected to fail"
    Left err         -> do assertEqual description expectedError err
  
runSuccessfulProgram program = do initial <- initialEnvironment
                                  programResult <- failableIO $ (runProgram program initial)
                                  case programResult of
                                    Right finalState -> return finalState
                                    Left err -> fail $ "Program failed: " ++ (show err)


testASimpleAssignment = TestCase
                        (assertSuccessfulExecutionResult 
                         "a simple assignment" 
                         "a=2"
                         initialBindings
                         [("a", NumericValue 2.0)])

testAMoreComplicatedAssignment = TestCase 
                                 (assertSuccessfulExecutionResult
                                  "a more complicated assignment" 
                                  "a=2 + 3 * 10"
                                  initialBindings 
                                  [("a", NumericValue 32.0)])

testAnExpressionWithAVariable = TestCase
                                (assertSuccessfulExecutionResult
                                 "assigning an expression that uses a variable" 
                                 "a=b+2"
                                 [("b", NumericValue 2.5)]
                                 [("a", NumericValue 4.5), ("b", NumericValue 2.5)])

testAnAssignmentError = TestCase
                        (assertFailingExecutionResult
                         "an assignment error"
                         "a = b + 2"
                         initialBindings
                         (DefaultError "Unassigned variable: b"))

testAPrintError = TestCase
                  (assertFailingExecutionResult
                   "a print error" 
                   "print a" 
                   initialBindings 
                   (DefaultError "Unassigned variable: a"))

testAWhileLoop = TestCase 
                 (assertSuccessfulExecutionResult 
                  "a while loop" 
                  (unlines ["a=1",
                            "while a<10",
                            "a=a+1",
                            "end"])
                  initialBindings 
                  [("a", NumericValue 10.0)])

testAFunctionCall = TestCase
                    (assertSuccessfulExecutionResult
                     "a function call"
                     (unlines ["whatever = {x| return x + 1}",
                               "a = whatever(1)"])
                     initialBindings
                     [("a", NumericValue 2.0)])

testFunctionWithMultipleArgs = TestCase
                               (assertSuccessfulExecutionResult
                                "a function with multiple arguments"
                                (unlines ["margs = {x, y| return x + y }",
                                          "a = margs(1, 2)"])
                                initialBindings
                                [("a", NumericValue 3.0)])

testIncompatibleArgTypes = TestCase
                           (assertFailingExecutionResult
                            "an expression with incompatible argument types"
                            "a = 1 + \"hello\""
                            initialBindings
                            (DefaultError (binaryOpFailureMessage "add" (NumericValue 1.0) (StringValue "hello"))))

testStringConcatenation = TestCase
                          (assertSuccessfulExecutionResult
                           "string concatenation"
                           "a = \"hello\" + \"world\""
                           initialBindings
                           [("a", StringValue "helloworld")])

testIncompatibleTypesSubtraction = TestCase
                                   (assertFailingExecutionResult
                                    "a subtraction with incompatible argument types"
                                    "a = 1 - \"hello\""
                                    initialBindings
                                    (DefaultError (binaryOpFailureMessage "subtract" (NumericValue 1.0) (StringValue "hello"))))

testStaticScoping = TestCase (do
                                resultState <- runSuccessfulProgram program
                                rValue <- failableIO $ getVariableValue resultState "r"
                                assertEqual "static scoping" rValue (Right $ NumericValue 3.0))
                              where
                                program = (unlines ["f = { y| return x + y }",
                                                    "x = 1",
                                                    "r = f(2)"])

testNestedStaticScoping = TestCase (do
                                      resultState <- runSuccessfulProgram program
                                      rValue <- failableIO $ getVariableValue resultState "r"
                                      assertEqual "clobbering a free variable" (Right (NumericValue 7.0)) rValue)
                                    where
                                      program = (unlines ["x = 1",
                                                          "f = {y|",
                                                          "       x = 2",
                                                          "       return {z| return x + y + z}}",
                                                          "r = f(2)(3)"])
                                                         
                                                          

testNotACallable = TestCase
                   (assertFailingExecutionResult
                    "trying to call something that is not a function"
                    "v = 1\nv(2)"
                    initialBindings
                    (DefaultError "Can't call NumericValue 1.0"))
          
                    
testCallingTheResultOfAFunction = TestCase (do resultState <- runSuccessfulProgram "a = { return {x| return x + 2 } }\nb = a()(2)"
                                               rValue <- failableIO $ getVariableValue resultState "b"
                                               assertEqual "calling the result of a function" (Right $ NumericValue 4.0) rValue)

testCreateAdder = TestCase
                  (assertSuccessfulExecutionResult
                   "creating an adder"
                   (unlines ["make_adder = {n| return {x| return x + n}}",
                             "a = make_adder(1)(2)"])
                   initialBindings
                   [("a", NumericValue 3.0)])

testIfStatement = TestCase 
        (assertSuccessfulExecutionResult 
         "an if statement" 
         (unlines ["if 1 < 2",
                   "a = 1",
                   "end"])
         initialBindings 
         [("a", NumericValue 1.0)])



evaluatorTests = TestList [testASimpleAssignment, 
                           testAMoreComplicatedAssignment, 
                           testAnAssignmentError, 
                           testAnExpressionWithAVariable, 
                           testAPrintError, 
                           testAFunctionCall, 
                           testAWhileLoop, 
                           testFunctionWithMultipleArgs, 
                           testIncompatibleArgTypes, 
                           testStringConcatenation, 
                           testIncompatibleTypesSubtraction, 
                           testStaticScoping, 
                           testNestedStaticScoping,
                           testNotACallable, 
                           testCallingTheResultOfAFunction, 
                           testCreateAdder,
                           testIfStatement]

main = do { runTestTT evaluatorTests }

