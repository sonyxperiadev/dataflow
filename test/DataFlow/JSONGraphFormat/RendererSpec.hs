{-# LANGUAGE OverloadedStrings #-}
module DataFlow.JSONGraphFormat.RendererSpec where

import Control.Monad (when)

import Data.Aeson ((.=), object, toJSON, ToJSON, encode)
import qualified Data.Aeson as A

import Data.Vector (fromList)
import qualified Data.Map as M
import Test.Hspec (Spec, describe, it, expectationFailure, Expectation)
import Data.ByteString.Lazy.Char8 (unpack)
import Text.Printf (printf)

import DataFlow.Core
import DataFlow.JSONGraphFormat.Renderer (convertDiagram)

shouldEncodeAsJSON :: (ToJSON a) => a -> A.Value -> Expectation
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
            "nodes" .= A.Array (fromList []),
            "edges" .= A.Array (fromList [])
          ]
        ]
    it "uses diagram title attribute as graph label" $
      convertDiagram (Diagram (M.singleton "title" (String "Foo")) [] []) `shouldEncodeAsJSON` object [
          "graph" .= object [
            "label" .= A.String "Foo",
            "metadata" .= object [],
            "nodes" .= A.Array (fromList []),
            "edges" .= A.Array (fromList [])
          ]
        ]
    it "converts nodes" $
      convertDiagram (Diagram M.empty [
                                  Node $ InputOutput "foo" $ M.fromList [
                                      ("title", String "Foo"),
                                      ("a", String "b")
                                    ],
                                  Node $ Function "bar" $ M.fromList [
                                      ("title", String "Bar"),
                                      ("c", String "d")
                                    ]
                                ] []) `shouldEncodeAsJSON` object [
        "graph" .= object [
          "nodes" .= A.Array (fromList [
            object [
              "id" .= A.String "foo",
              "label" .= A.String "Foo",
              "metadata" .= object [
                "type" .= A.String "io",
                "a" .= A.String "b"
              ]
            ],
            object [
              "id" .= A.String "bar",
              "label" .= A.String "Bar",
              "metadata" .= object [
                "type" .= A.String "function",
                "c" .= A.String "d"
                ]
              ]
            ]),
          "edges" .= A.Array (fromList []),
          "metadata" .= object []
        ]
      ]
    it "converts edges" $
      convertDiagram (Diagram M.empty [] [
        Flow "a" "b" $ M.fromList [
          ("title", String "Foo"),
          ("a", String "b")
        ],
        Flow "b" "c" $ M.fromList [
          ("title", String "Bar"),
          ("b", String "c")
        ]
      ]) `shouldEncodeAsJSON` object [
        "graph" .= object [
          "nodes" .= A.Array (fromList []),
          "edges" .= A.Array (fromList [
            object [
              "source" .= A.String "a",
              "target" .= A.String "b",
              "label" .= A.String "Foo",
              "metadata" .= object [
                "a" .= A.String "b"
              ]
            ],
            object [
              "source" .= A.String "b",
              "target" .= A.String "c",
              "label" .= A.String "Bar",
              "metadata" .= object [
                "b" .= A.String "c"
              ]
            ]
          ]),
          "metadata" .= object []
        ]
      ]
    it "adds boundary id as node metadata if available" $
      convertDiagram (Diagram M.empty [
                                  TrustBoundary "foo" M.empty [
                                    InputOutput "bar" M.empty
                                  ]
                                ] []) `shouldEncodeAsJSON` object [
        "graph" .= object [
          "nodes" .= A.Array (fromList [
            object [
              "id" .= A.String "bar",
              "metadata" .= object [
                "type" .= A.String "io",
                "trust-boundary" .= A.String "foo"
              ]
            ]
            ]),
          "edges" .= A.Array (fromList []),
          "metadata" .= object []
        ]
      ]
