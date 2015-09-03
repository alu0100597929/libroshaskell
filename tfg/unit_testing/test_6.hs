import Cap_6_module.Repl
import Test.Hspec

-- Recuerda: escapar todo lo que pueda dar problemas en las strings!

main = hspec $ do
  describe "validate" $ do
    it "the add function works" $ do
      result <- evalString "(+ 2 3)"
      result `shouldBe` "5"
    it "this is not a recognized thing" $ do
      result <- evalString "(cons this '())"
      result `shouldBe` "Unrecognized special form: this"
    it "cons function works" $ do
      result <- evalString "(cons 2 3)"
      result `shouldBe` "(2 . 3)"
    it "cons works with an atom and the empty list" $ do
      result <- evalString "(cons 'this '())"
      result `shouldBe` "(this)"