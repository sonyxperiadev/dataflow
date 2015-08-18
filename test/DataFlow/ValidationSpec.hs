module DataFlow.ValidationSpec where

import Data.Map as M

import Test.Hspec

import DataFlow.Core
import DataFlow.Validation

shouldAccept :: Diagram -> Expectation
shouldAccept d = either onFailure onSuccess (validate d)
  where onSuccess _ = return ()
        onFailure e = expectationFailure $
                        "Expected diagram to validate: " ++ show e

shouldReject :: Diagram -> Expectation
shouldReject d = either onFailure onSuccess (validate d)
  where onSuccess _ = expectationFailure $
                        "Expected diagram to be rejected by validate: " ++ show d
        onFailure _ = return ()

spec :: Spec
spec =
  describe "validate" $ do
    it "should accept an empty diagram" $
      shouldAccept $ Diagram M.empty [] []
    it "should reject a diagram with flows between non-existing nodes" $
      shouldReject $ Diagram M.empty [] [
        Flow "foo" "bar" M.empty
      ]
    it "should accept a diagram with flows between existing nodes" $
      shouldAccept $ Diagram M.empty [
        Node $ Function "foo" M.empty,
        Node $ Function "bar" M.empty
      ] [
        Flow "foo" "bar" M.empty
      ]
    it "should accept a diagram with flows between existing nodes inside a boundary" $
      shouldAccept $ Diagram M.empty [
        TrustBoundary "foo" M.empty [
          Function "bar" M.empty,
          Function "baz" M.empty
        ]
      ] [
        Flow "bar" "baz" M.empty
      ]
    it "should reject a diagram with flows between boundary IDs" $
      shouldReject $ Diagram M.empty [
        TrustBoundary "foo" M.empty [],
        TrustBoundary "bar" M.empty []
      ] [
        Flow "foo" "bar" M.empty
      ]
    it "should reject a boundaries with equal IDs" $
      shouldReject $ Diagram M.empty [
        TrustBoundary "foo" M.empty [],
        TrustBoundary "foo" M.empty []
      ] []
