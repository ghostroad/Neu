module Neu.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Neu.AST

languageDef = (haskellStyle { reservedOpNames = ["*", "+", "/", "-", "=", ">"],
                                reservedNames   = ["print", "load", "while", "def", "end", "return", "if"] })

lexer = P.makeTokenParser languageDef

whiteSpace            = P.whiteSpace lexer
lexeme                = P.lexeme lexer
symbol                = P.symbol lexer
natural               = P.natural lexer
parens                = P.parens lexer
braces                = P.braces lexer
comma                 = P.comma lexer
semi                  = P.semi lexer
identifier            = P.identifier lexer
reserved              = P.reserved lexer
reservedOp            = P.reservedOp lexer
stringLiteral         = P.stringLiteral lexer

factor = parens neuValueParser
        <|> numericLiteral
        <|> try functionCallExpression
        <|> variableReference
        <|> functionDefinition
        <|> stringLiteralParser
        <?> "expression"

table = [[op ">" (Comparison GreaterThan) AssocNone, op "<" (Comparison LessThan) AssocNone],
         [op "*" (Multiplication) AssocLeft, op "/" (Division) AssocLeft],
         [op "+" (Addition) AssocLeft, op "-" (Subtraction) AssocLeft]]
        where
          op s f assoc
             = Infix (do{ reservedOp s; return f} <?> "operator") assoc

neuValueParser         = buildExpressionParser table factor <?> "arithmetic expression"
                       
numericLiteral         = do x <- natural
                            return (Const (fromIntegral x)) <?> "numeric literal"

stringLiteralParser    = do x <- stringLiteral
                            return (StringLiteral x) <?> "string literal"

variableReference      = do x <- identifier
                            return (Variable x) <?> "identifier"

functionCallExpression = do f <- (variableReference <|> functionDefinition)
                            applications <- many1 (parens $ sepBy neuValueParser comma)
                            return (assembleFunctionCall applications f)
                            <?> "function call"

assembleFunctionCall (x:xs) func = foldr (\a b -> FunctionCall a b) (FunctionCall x func) xs

createAssignmentStatement (Variable identifier) value = AssignExpression identifier value
    
assignmentStatement  = do v <- variableReference
                          reservedOp "="
                          e <- neuValueParser
                          return (createAssignmentStatement v e)
                          <?> "assignment statement"
                     
printStatement       = do reserved "print"
                          e <- neuValueParser
                          return (Print e)
                          <?> "print statement"

returnStatement      = do reserved "return"
                          e <- neuValueParser
                          return (Return e)
                          <?> "return statement"
                    
functionArguments    = do arguments <- sepBy identifier comma
                          symbol "|"
                          return arguments
                          <?> "function argument declaration"

functionDefinition   = braces $ do args <- option [] (try functionArguments)
                                   statements <- many1 statementParser 
                                   return (FunctionDefinition args statements) <?> "function definition"

loadStatement	     = do reserved "load"
                          fileName <- stringLiteral
                          return (Load fileName) <?> "load statement"

whileLoop            = do reserved "while"
                          condition <- neuValueParser
                          statements <- manyTill statementParser (reserved "end")
                          return (WhileLoop condition statements) <?> "while loop"

ifStatement          = do reserved "if"
                          condition <- neuValueParser
                          statements <- manyTill statementParser (reserved "end")
                          return (IfStatement condition statements) <?> "if statement"

expressionStatement  = do expression <- neuValueParser
                          return (ExpressionStatement expression) <?> "an expression"

statementParser = (try assignmentStatement) <|> expressionStatement <|> printStatement <|> loadStatement <|> whileLoop <|> ifStatement <|> returnStatement

whitespaceStrippingParser parser = do whiteSpace
                                      x <- parser
                                      eof
                                      return x
                                     
programParser = whitespaceStrippingParser (many1 statementParser)

parseNeuProgram programSource =  parse programParser "" programSource