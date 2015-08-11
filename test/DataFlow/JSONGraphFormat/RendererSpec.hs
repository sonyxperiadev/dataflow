{-# LANGUAGE OverloadedStrings #-}
module DataFlow.JSONGraphFormat.RendererSpec where

import Control.Monad (when)
import Data.Aeson
import Data.Vector (fromList)
import qualified Data.Map as M
import Test.Hspec (Spec, describe, it, expectationFailure, Expectation)
import Data.ByteString.Lazy.Char8 (unpack)
import Text.Printf (printf)

import DataFlow.Core
import DataFlow.JSONGraphFormat.Renderer

shouldEncodeAsJSON :: (ToJSON a) => a -> Value -> Expectation
v `shouldEncodeAsJSON` e = do
  let j = toJSON v
  when (j /= e) $
    expectationFailure $
      printf "expected:\n%s\nbut got:\n%s\n" (unpack $ encode e) (unpack $ encode j)

spec :: Spec
spec =
  describe "renderJSONGraph" $ do

    it "converts an empty diagram" $
      convertDiagram (Diagram M.empty [] []) `shouldEncodeAsJSON` object [
          "graph" .= object [
            "metadata" .= object [],
            "nodes" .= Array (fromList []),
            "edges" .= Array (fromList [])
          ]
        ]
    it "uses diagram title attribute as graph label" $
      convertDiagram (Diagram (M.singleton "title" "Foo") [] []) `shouldEncodeAsJSON` object [
          "graph" .= object [
            "label" .= String "Foo",
            "metadata" .= object [],
            "nodes" .= Array (fromList []),
            "edges" .= Array (fromList [])
          ]
        ]
    it "converts nodes" $
      convertDiagram (Diagram M.empty [
                                  Node $ InputOutput "foo" $ M.fromList [
                                      ("title", "Foo"),
                                      ("a", "b")
                                    ],
                                  Node $ Function "bar" $ M.fromList [
                                      ("title", "Bar"),
                                      ("c", "d")
                                    ]
                                ] []) `shouldEncodeAsJSON` object [
        "graph" .= object [
          "nodes" .= Array (fromList [
            object [
              "id" .= String "foo",
              "label" .= String "Foo",
              "metadata" .= object [
                "type" .= String "io",
                "a" .= String "b"
              ]
            ],
            object [
              "id" .= String "bar",
              "label" .= String "Bar",
              "metadata" .= object [
                "type" .= String "function",
                "c" .= String "d"
                ]
              ]
            ]),
          "edges" .= Array (fromList []),
          "metadata" .= object []
        ]
      ]
    it "converts edges" $
      convertDiagram (Diagram M.empty [] [
        Flow "a" "b" $ M.fromList [
          ("title", "Foo"),
          ("a", "b")
        ],
        Flow "b" "c" $ M.fromList [
          ("title", "Bar"),
          ("b", "c")
        ]
      ]) `shouldEncodeAsJSON` object [
        "graph" .= object [
          "nodes" .= Array (fromList []),
          "edges" .= Array (fromList [
            object [
              "source" .= String "a",
              "target" .= String "b",
              "label" .= String "Foo",
              "metadata" .= object [
                "a" .= String "b"
              ]
            ],
            object [
              "source" .= String "b",
              "target" .= String "c",
              "label" .= String "Bar",
              "metadata" .= object [
                "b" .= String "c"
              ]
            ]
          ]),
          "metadata" .= object []
        ]
      ]
    it "adds boundary title as node metadata if available" $
      convertDiagram (Diagram M.empty [
                                  TrustBoundary (M.singleton "title" "Foo") [
                                    InputOutput "bar" M.empty
                                  ]
                                ] []) `shouldEncodeAsJSON` object [
        "graph" .= object [
          "nodes" .= Array (fromList [
            object [
              "id" .= String "bar",
              "metadata" .= object [
                "type" .= String "io",
                "trust-boundary" .= String "Foo"
              ]
            ]
            ]),
          "edges" .= Array (fromList []),
          "metadata" .= object []
        ]
      ]
