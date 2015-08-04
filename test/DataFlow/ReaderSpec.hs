module DataFlow.ReaderSpec where

import Control.Monad (when)
import Data.Map as M
import Test.Hspec
import Text.Printf
import DataFlow.Core
import DataFlow.Reader

parseFailure :: Show e => String -> Diagram -> e -> Expectation
parseFailure input expected err =
  expectationFailure $
    printf "input:\n%s\nexpected: %s\n  but parsing failed with error: %s"
           input
           (show expected)
           (show err)

checkEquality :: (Eq a, Show a) => a -> a -> Expectation
checkEquality e a =
  when (a /= e) $
    expectationFailure $
      printf "  expected: %s\n  but got: %s " (show e) (show a)

shouldReadAs :: String -> Diagram -> Expectation
s `shouldReadAs` expected =
  either (parseFailure s expected) (checkEquality expected) (readDiagram "test input" s)

shouldFailRead :: String -> Expectation
shouldFailRead s =
  either onFailure onSuccess (readDiagram "test input" s)
  where onFailure _ = return ()
        onSuccess d =
          expectationFailure $
            printf "Expected read to fail, but got: %s" (show d)
spec :: Spec
spec =
  describe "readDiagram" $ do

    it "reads empty diagram" $
      "diagram {}" `shouldReadAs` Diagram M.empty []

    it "reads diagram with single attribute" $
      "diagram { name = \"\" }" `shouldReadAs` Diagram (M.singleton "name" "") []

    it "reads diagram with multiple attributes" $
      let input = unlines [
                            "diagram {",
                            "  name = \"foo\"",
                            "  importance = \"high\"",
                            "}"
                          ]
      in input `shouldReadAs` Diagram (M.fromList [("name", "foo"),
                                                   ("importance", "high")]) []


    it "reads diagram with whitespace inside braces" $
      "diagram {\n   \n    }" `shouldReadAs` Diagram M.empty []

    it "reads diagram with trust boundary" $
      let input = unlines [
                            "diagram {",
                            "  boundary {}",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          TrustBoundary M.empty []
        ]
    it "reads diagram with trust boundary and nested objects" $
      let input = unlines [
                            "diagram {",
                            "  boundary {",
                            "    io dynamo",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          TrustBoundary M.empty [
            InputOutput "dynamo" M.empty
          ]
        ]
    it "reads diagram with function" $
      let input = unlines [
                            "diagram {",
                            "  function server",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          Function "server" M.empty
        ]
    it "reads diagram with database" $
      let input = unlines [
                            "diagram {",
                            "  database dynamo",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          Database "dynamo" M.empty
        ]
    it "reads diagram with io" $
      let input = unlines [
                            "diagram {",
                            "  io analytics",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          InputOutput "analytics" M.empty
        ]
    it "reads diagram with flow" $
      let input = unlines [
                            "diagram {",
                            "  a -> b",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          Flow "a" "b" M.empty
        ]
    it "does not allow multiline string" $
      let input = unlines [
                            "diagram {",
                            "  foo = \"",
                            "  omg",
                            "  yes",
                            "  \"",
                            "}"
                          ]
      in shouldFailRead input
    it "reads attributes" $
      let input = unlines [
                            "diagram {",
                            "  io baz {",
                            "    title = \"foo\"",
                            "    description = \"bar\"",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          InputOutput "baz" (M.fromList [("title", "foo"), ("description", "bar")])
        ]
    it "reads attributes and objects" $
      let input = unlines [
                            "diagram {",
                            "  name = \"bar\"",
                            "  io baz {",
                            "    title = \"foo\"",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAs` Diagram (M.singleton "name" "bar") [
          InputOutput "baz" (M.singleton "title" "foo")
        ]
    it "reads flow with attributes" $
      let input = unlines [
                            "diagram {",
                            "  foo -> bar {",
                            "    title = \"baz\"",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAs` Diagram M.empty [
          Flow "foo" "bar" (M.singleton "title" "baz")
        ]


