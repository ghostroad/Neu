module Neu.AST where

import Data.Map as Map
import Data.IORef
import Control.Monad.Error

data NeuError = DefaultError String deriving (Show, Eq)

type ThrowsError = Either NeuError

instance Error NeuError where
     noMsg  = DefaultError "An error has occurred"
     strMsg = DefaultError


initialEnvironment = newEnvironmentFrom []

newEnvironmentFrom :: [(String, Value)] -> IO (IORef (Map String Value))
newEnvironmentFrom map = newIORef (Map.fromList map)

instance Show Environment where
    show env = "<environment>"
data Value = NumericValue Float | StringValue String | BoolValue Bool | Function [Identifier] NeuProgram Environment | Null
              deriving (Show, Eq)

data RelationalOperator = GreaterThan | LessThan deriving (Show, Eq)

type Identifier       = String
type Environment      = IORef (Map Identifier Value)
data ExecutionResult  = ReturnResult Value Environment | ContinueResult Environment
type NeuProgram       = [Statement]

data Expression =  Const Float |
                   StringLiteral String |
                   Variable Identifier |
                   Addition Expression Expression |
                   Multiplication Expression Expression |
                   Subtraction Expression Expression |
                   Division Expression Expression |
                   Comparison RelationalOperator Expression Expression |
                   FunctionDefinition [Identifier] NeuProgram |
                   FunctionCall [Expression] Expression
                   deriving (Eq, Show)

data Statement = AssignExpression Identifier Expression |
                 Print Expression |
                 Load FilePath |
                 WhileLoop Expression NeuProgram |
                 IfStatement Expression NeuProgram |
                 Return Expression |
                 ExpressionStatement Expression
                 deriving (Eq, Show)
