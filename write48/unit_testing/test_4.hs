import Cap_4_module.Error_checking
import Test.Hspec
import Control.Monad

-- Recuerda: escapar todo lo que pueda dar problemas en las strings!

main = hspec $ do
  describe "validate" $ do
    it "can sum all the elements in a list" $ do
      evaled <- return $ liftM show $ readExpr "(+ 1 2 2)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "5"
    it "eval strings maintaining that strings" $ do
      evaled <- return $ liftM show $ readExpr "\"a string\"" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "\"a string\""
    it "eval numbers maintaining that numbers" $ do
      evaled <- return $ liftM show $ readExpr "2" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "2"
    it "eval atoms erasing its quotes" $ do
      evaled <- return $ liftM show $ readExpr "'atom" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "atom"