module DataFlow.Assertions where

import Control.Monad (when)
import Test.Hspec
import Text.ParserCombinators.Parsec
import Text.Printf

import DataFlow.Core
import DataFlow.Reader

parseFailure :: (Show a, Show e) => String -> a -> e -> Expectation
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

shouldReadAs :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
shouldReadAs p s expected =
  either (parseFailure s expected) (checkEquality expected) (parse p "test input" s)

shouldReadAsDiagram :: String -> Diagram -> Expectation
s `shouldReadAsDiagram` expected = shouldReadAs document s expected

shouldFailReadAs :: (Eq a, Show a) => Parser a -> String -> Expectation
shouldFailReadAs p s =
  either onFailure onSuccess (parse p "test input" s)
  where onFailure _ = return ()
        onSuccess d =
          expectationFailure $
            printf "Expected read to fail, but got: %s" (show d)

shouldFailReadAsDiagram :: String -> Expectation
shouldFailReadAsDiagram = shouldFailReadAs document

