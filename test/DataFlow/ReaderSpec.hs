module DataFlow.ReaderSpec where

import Data.Map as M

import Test.Hspec

import DataFlow.Assertions
import DataFlow.Core

spec :: Spec
spec =
  describe "readDiagram" $ do

    it "reads empty diagram" $
      "diagram {}" `shouldReadAsDiagram` Diagram M.empty []

    it "reads diagram with single attribute" $
      "diagram { name = \"\" }" `shouldReadAsDiagram` Diagram (M.singleton "name" "") []

    it "reads diagram with multiple attributes" $
      let input = unlines [
                            "diagram {",
                            "  name = \"foo\"",
                            "  importance = \"high\"",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram (M.fromList [("name", "foo"),
                                                   ("importance", "high")]) []


    it "reads diagram with whitespace inside braces" $
      "diagram {\n   \n    }" `shouldReadAsDiagram` Diagram M.empty []

    it "reads diagram with trust boundary" $
      let input = unlines [
                            "diagram {",
                            "  boundary {}",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram M.empty [
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
      in input `shouldReadAsDiagram` Diagram M.empty [
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
      in input `shouldReadAsDiagram` Diagram M.empty [
          Function "server" M.empty
        ]
    it "reads diagram with database" $
      let input = unlines [
                            "diagram {",
                            "  database dynamo",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram M.empty [
          Database "dynamo" M.empty
        ]
    it "reads diagram with io" $
      let input = unlines [
                            "diagram {",
                            "  io analytics",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram M.empty [
          InputOutput "analytics" M.empty
        ]
    it "reads diagram with flow" $
      let input = unlines [
                            "diagram {",
                            "  a -> b",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram M.empty [
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
      in shouldFailReadAsDiagram input
    it "reads attributes" $
      let input = unlines [
                            "diagram {",
                            "  io baz {",
                            "    title = \"foo\"",
                            "    description = \"bar\"",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram M.empty [
          InputOutput "baz" (M.fromList [("title", "foo"), ("description", "bar")])
        ]
    it "reads multiple attributes on a single line" $
      let input = unlines [
                            "diagram {",
                            "  io baz {",
                            "    title = \"foo\" description = \"bar\"",
                            "  }",
                            "}"
                          ]
      in input `shouldReadAsDiagram` Diagram M.empty [
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
      in input `shouldReadAsDiagram` Diagram (M.singleton "name" "bar") [
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
      in input `shouldReadAsDiagram` Diagram M.empty [
          Flow "foo" "bar" (M.singleton "title" "baz")
        ]
