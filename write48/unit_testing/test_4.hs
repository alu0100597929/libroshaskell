import Cap_4_module.Error_checking
import Test.Hspec

-- Recuerda: escapar todo lo que pueda dar problemas en las strings!

main = hspec $ do
  describe "validate" $ do
    it "can sum all the elements in a list" $ do
      (showVal . eval . readExpr) "(+ 1 2 2)" `shouldBe` "5"
    it "eval strings maintaining that strings" $ do
      (showVal . eval . readExpr) "\"a string\"" `shouldBe` "\"a string\""
    it "eval numbers maintaining that numbers" $ do
      (showVal . eval . readExpr) "2" `shouldBe` "2"
    it "eval atoms erasing its quotes" $ do
      (showVal . eval . readExpr) "'atom" `shouldBe` "atom"