import Cap_2_modules.Recursive_Parser
import Test.Hspec

-- Recuerda: escapar todo lo que pueda dar problemas en las strings!

main = hspec $ do
  describe "validate" $ do
    it "recognizes simple lists" $ do
      readExpr "(a test)" `shouldBe` "Found value: List [Atom \"a\",Atom \"test\"]"
    it "recognizes nested lists" $ do
      readExpr "(a (nested) test)" `shouldBe` "Found value: List [Atom \"a\",List [Atom \"nested\"],Atom \"test\"]"
    it "recognizes nested dotted lists" $ do
      readExpr "(a (dotted . list) test)" `shouldBe` "Found value: List [Atom \"a\",DottedList [Atom \"dotted\"] (Atom \"list\"),Atom \"test\"]"
    it "recognizes quoted dotted lists" $ do
      readExpr "(a '(quoted (dotted . list)) test)" `shouldBe` "Found value: List [Atom \"a\",List [Atom \"quote\",List [Atom \"quoted\",DottedList [Atom \"dotted\"] (Atom \"list\")]],Atom \"test\"]"
    it "doesn't recognize imbalanced parenthesis" $ do
      readExpr "(a '(imbalanced parens)" `shouldBe` "No match: \"lisp\" (line 1, column 24):\nunexpected end of input\nexpecting space, \".\" or \")\""