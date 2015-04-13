module DataFlow.Graphviz.RendererSpec where

import Test.Hspec

import DataFlow.Graphviz
import DataFlow.Graphviz.Renderer

spec :: Spec
spec =
  describe "renderGraphviz" $ do

    it "renders digraph id" $
      renderGraphviz (Digraph (ID "g") []) `shouldBe` "digraph g {\n}\n"

    it "renders digraph with a node stmt" $
      renderGraphviz (Digraph (ID "g") [
          NodeStmt (ID "n") []
        ]) `shouldBe` "digraph g {\n  n\n}\n"

    it "renders digraph with an edge stmt" $
      renderGraphviz (Digraph (ID "g") [
          EdgeStmt (EdgeExpr
                      (IDOperand $ NodeID (ID "n1") Nothing)
                      Arrow
                      (IDOperand $ NodeID (ID "n2") Nothing))
          []
        ]) `shouldBe` "digraph g {\n  n1 -> n2;\n}\n"

    it "renders digraph with an attr stmt" $
      renderGraphviz (Digraph (ID "g") [
          AttrStmt Graph []
        ]) `shouldBe` "digraph g {\n  graph []\n}\n"

    it "renders digraph with an equals stmt" $
      renderGraphviz (Digraph (ID "g") [
          EqualsStmt (ID "i1") (ID "i2")
        ]) `shouldBe` "digraph g {\n  i1 = i2;\n}\n"

    it "renders digraph with a subgraph stmt" $
      renderGraphviz (Digraph (ID "g") [
          SubgraphStmt $ Subgraph (ID "sg") []
        ]) `shouldBe` "digraph g {\n  subgraph sg {}\n}\n"
