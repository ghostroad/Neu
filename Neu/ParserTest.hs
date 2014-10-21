module Neu.ParserTest(parserTests) where
import Test.HUnit
import Neu.Parser
import Neu.AST

parseTestNeuProgram program  = case (parseNeuProgram program) of
                                 Left err -> Left (show err)
                                 Right parsedStatements -> Right parsedStatements

assertProgramParseTree description programSource neuAST = TestCase (assertEqual description (Right neuAST) (parseTestNeuProgram programSource))

test1 = assertProgramParseTree 
        "parse a simple print statement" 
        "print 1" 
        [Print (Const 1.0)]

test2 = assertProgramParseTree 
        "parse an arithmetic expression" 
        "a = 1 * 2 + 5" 
        [AssignExpression "a" (Addition (Multiplication (Const 1.0) (Const 2.0)) (Const 5.0))]

test3 = assertProgramParseTree 
        "parse a load statement" 
        "load \"program1.neu\"" 
        [Load "program1.neu"]

test4 = assertProgramParseTree 
        "a boolean expression" 
        "a = 1\nb=a > 4" 
        [(AssignExpression "a" (Const 1.0)), (AssignExpression "b" (Comparison GreaterThan (Variable "a") (Const 4.0)))]

test5 = assertProgramParseTree 
        "a less-than comparison" 
        "a = 1\nb=a < 4" 
        [(AssignExpression "a" (Const 1.0)), (AssignExpression "b" (Comparison LessThan (Variable "a") (Const 4.0)))]

test6 = assertProgramParseTree 
        "a while loop" 
        "a = 1\nwhile a < 10\nprint a\na=a+1\nend" 
        [ AssignExpression "a" (Const 1.0),
                           WhileLoop (Comparison LessThan (Variable "a")
                                                     (Const 10.0)) [Print (Variable "a"), 
                                                                          AssignExpression "a" (Addition (Variable "a") (Const 1.0))]]

testIfStatement = assertProgramParseTree


test7 = assertProgramParseTree
        "a function definition"
        "whatever = {a| print a\n return 1\n}" 
        [ AssignExpression "whatever" (FunctionDefinition ["a"] [Print (Variable "a"), Return (Const 1.0)]) ] 

test8 = assertProgramParseTree
        "a function with no arguments"
        "whatever = { print 1 }"
        [ AssignExpression "whatever" (FunctionDefinition [] [Print (Const 1.0)]) ]

test9 = assertProgramParseTree
        "a function call"
        "a = 1 + whatever(2 + b, 3)"
        [ AssignExpression "a" (Addition (Const 1.0) (FunctionCall [(Addition (Const 2.0) (Variable "b")), Const 3.0] (Variable "whatever"))) ]

test10 = assertProgramParseTree
         "an arithmetic operation with strange argument types"
         "a = 1 + \"hello\""
         [ AssignExpression "a" (Addition (Const 1.0) (StringLiteral "hello")) ]

test11 = assertProgramParseTree
         "string concatenation"
         "a = \"hello\" + \"world\""
         [ AssignExpression "a" (Addition (StringLiteral "hello") (StringLiteral "world")) ]

test12 = assertProgramParseTree
         "expressions as statements"
         "whatever(1)"
         [ ExpressionStatement (FunctionCall [Const 1.0] (Variable "whatever")) ]

test13 = assertProgramParseTree
         "calling the result of a function"
         "whatever(1)(2)"
         [ ExpressionStatement (FunctionCall [Const 2.0] (FunctionCall [Const 1.0] (Variable "whatever"))) ]

test14 = assertProgramParseTree
         "a multiline function definition"
         (unlines ["func = { ",
                   "         i = 1",
                   "         print \"hello\"",
                   "         print \"world\"",
                   "}"])
         [ AssignExpression "func" (FunctionDefinition [] [AssignExpression "i" (Const 1.0), Print (StringLiteral "hello"), Print (StringLiteral "world")]) ]

test15 = assertProgramParseTree
         "a function that performs an assignment"
         "{\n x = 1 }"
         [ ExpressionStatement (FunctionDefinition [] [AssignExpression "x" (Const 1.0)]) ]

test16 = assertProgramParseTree
         "an if statement"
         (unlines ["if a < 10",
                   "  print 2 ",
                   "end"])
         [ IfStatement (Comparison LessThan (Variable "a") (Const 10.0)) [Print (Const 2.0)] ]


parserTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16]

main = do { runTestTT parserTests }