import Test.HUnit
import Neu.Parser
import Neu.ParserTest
import Neu.EvaluatorTest

tests = TestList [parserTests, evaluatorTests]

main = do { runTestTT tests }
