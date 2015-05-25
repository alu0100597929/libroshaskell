import Cap_2_modules.Simple_Parser
import Test.Hspec

main = hspec $ do
  describe "validate" $ do 
    it "recognizes strings (between \"\")" $ do
      readExpr "\"this is a string\"" `shouldBe` "Found value: String \"this is a string\""
    it "recognizes numbers (integers)" $ do
      readExpr "25" `shouldBe` "Found value: Number 25"
    it "recognizes atoms" $ do
      readExpr "symbol" `shouldBe` "Found value: Atom \"symbol\""
    it "doesn't recognize parenthesis" $ do
      readExpr "(symbol)" `shouldBe` "No match: \"lisp\" (line 1, column 1):\nunexpected \"(\"\nexpecting letter, \"\\\"\", \"#\\\\\", \"#\" or digit"