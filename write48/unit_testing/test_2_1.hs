import Cap_2_tests.Simple_Parser
import Test.Hspec

main = hspec $ do
  describe "validate" $ do 
    it "123 is not valid" $ do
      readExpr "\"this is a string\"" `shouldBe` "Found value: String \"this is a string\""
    it "1 is not valid" $ do
      readExpr "25" `shouldBe` "Found value: Number 25"
    it "2121 is valid" $ do
      readExpr "symbol" `shouldBe` "Found value: Atom \"symbol\""
    it "2121 is valid" $ do
      readExpr "(symbol)" `shouldBe` "No match: \"lisp\" (line 1, column 1):\nunexpected \"(\"\nexpecting letter, \"\\\"\", \"#\\\\\", \"#\" or digit"