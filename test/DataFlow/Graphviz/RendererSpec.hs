module DataFlow.Graphviz.RendererSpec where

import Test.Hspec

import DataFlow.Graphviz
import DataFlow.Graphviz.Renderer

spec :: Spec
spec =
  describe "renderGraphviz" $ do

    it "renders digraph id" $
      renderGraphviz (Digraph "g" []) `shouldBe` "digraph g {\n}\n"

    it "renders digraph with a node stmt" $
      renderGraphviz (Digraph "g" [
          NodeStmt "n" []
        ]) `shouldBe` "digraph g {\n  n\n}\n"

    it "renders digraph with an edge stmt" $
      renderGraphviz (Digraph "g" [
          EdgeStmt (EdgeExpr
                      (IDOperand $ NodeID "n1" Nothing)
                      Arrow
                      (IDOperand $ NodeID "n2" Nothing))
          []
        ]) `shouldBe` "digraph g {\n  n1 -> n2;\n}\n"

    it "renders digraph with an attr stmt" $
      renderGraphviz (Digraph "g" [
          AttrStmt Graph []
        ]) `shouldBe` "digraph g {\n  graph []\n}\n"

    it "renders digraph with an equals stmt" $
      renderGraphviz (Digraph "g" [
          EqualsStmt "i1" "i2"
        ]) `shouldBe` "digraph g {\n  i1 = i2;\n}\n"

    it "renders digraph with a subgraph stmt" $
      renderGraphviz (Digraph "g" [
          SubgraphStmt $ Subgraph "sg" []
        ]) `shouldBe` "digraph g {\n  subgraph sg {}\n}\n"

    it "converts newlines to <br/>" $
      renderGraphviz (Digraph "g" [
          AttrStmt Graph [
            Attr "hello" "foo\nbar"
          ]
        ]) `shouldBe` "digraph g {\n  graph [\n    hello = foo<br/>bar;\n  ]\n}\n"
