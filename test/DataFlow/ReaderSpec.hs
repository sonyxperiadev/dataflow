module DataFlow.ReaderSpec where

import Control.Monad (when)
import Test.Hspec
import Text.Printf
import DataFlow.Core
import DataFlow.Reader

parseFailure :: Show e => Diagram -> e -> Expectation
parseFailure expected err =
  expectationFailure $
    printf "  expected: %s\n  but parsing failed with error: %s"
           (show expected)
           (show err)

checkEquality :: Diagram -> Diagram -> Expectation
checkEquality e a =
  when (a /= e) $
    expectationFailure $
      printf "  expected: %s\n  but got: %s " (show e) (show a)

shouldReadAs :: String -> Diagram -> Expectation
s `shouldReadAs` expected =
  either (parseFailure expected) (checkEquality expected) (readDiagram "test" s)


spec :: Spec
spec =
  describe "readDiagram" $ do

    it "reads unnamed diagram with {}" $
      "diagram {}" `shouldReadAs` Diagram Nothing []

    it "reads named with empty name diagram" $
      "diagram '' {}" `shouldReadAs` Diagram (Just "") []

    it "reads named diagram" $
      "diagram 'My Diagram' {}" `shouldReadAs` Diagram (Just "My Diagram") []

    it "reads diagram with whitespace inside braces" $
      "diagram {\n   \n    }" `shouldReadAs` Diagram Nothing []

    it "reads diagram with trust boundary" $
      let input = unlines [
                            "diagram {",
                            "  boundary 'AWS!' {}",
                            "}"
                          ]
      in input `shouldReadAs` Diagram Nothing [
          TrustBoundary "aws" "AWS!" []
        ]
    it "reads diagram with trust boundary and nested objects" $
      let input = unlines [
                            "diagram {",
                            "  boundary 'AWS!' {",
                            "    io dynamo 'DynamoDB'",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAs` Diagram Nothing [
          TrustBoundary "aws" "AWS!" [
            InputOutput "dynamo" "DynamoDB"
          ]
        ]
    it "reads diagram with function" $
      let input = unlines [
                            "diagram {",
                            "  function server 'Web Server'",
                            "}"
                          ]
      in input `shouldReadAs` Diagram Nothing [
          Function "server" "Web Server"
        ]
    it "reads diagram with database" $
      let input = unlines [
                            "diagram {",
                            "  database dynamo 'DynamoDB'",
                            "}"
                          ]
      in input `shouldReadAs` Diagram Nothing [
          Database "dynamo" "DynamoDB"
        ]
    it "reads diagram with io" $
      let input = unlines [
                            "diagram {",
                            "  io analytics 'Analytics'",
                            "}"
                          ]
      in input `shouldReadAs` Diagram Nothing [
          InputOutput "analytics" "Analytics"
        ]
    it "reads diagram with flow" $
      let input = unlines [
                            "diagram {",
                            "  a -> b 'Save' 'Stuff'",
                            "}"
                          ]
      in input `shouldReadAs` Diagram Nothing [
          Flow "a" "b" "Save" "Stuff"
        ]
