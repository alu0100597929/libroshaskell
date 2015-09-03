import Cap_5_module.Evaluation_2
import Test.Hspec
import Control.Monad

-- Recuerda: escapar todo lo que pueda dar problemas en las strings!

main = hspec $ do
  describe "validate" $ do
    it "can sum all the elements in a list" $ do
      evaled <- return $ liftM show $ readExpr "(< 2 3)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#t"
    it "eval strings maintaining that strings" $ do
      evaled <- return $ liftM show $ readExpr "(> 2 3)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#f"
    it "eval strings maintaining that strings" $ do
      evaled <- return $ liftM show $ readExpr "(>= 3 3)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#t"
    it "eval numbers maintaining that numbers" $ do
      evaled <- return $ liftM show $ readExpr "(string=? \"test\"  \"test\")" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#t"
    it "eval atoms erasing its quotes" $ do
      evaled <- return $ liftM show $ readExpr "(string<? \"abc\" \"bba\")" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#t"
    it "cdr function works" $ do
      evaled <- return $ liftM show $ readExpr "(cdr '(a simple test))" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "(simple test)"
    it "cdr and car functions works" $ do
      evaled <- return $ liftM show $ readExpr "(car (cdr '(a simple test)))" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "simple"
    it "car can return a list as head" $ do
      evaled <- return $ liftM show $ readExpr "(car '((this is) a test))" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "(this is)"
    it "cons function works" $ do
      evaled <- return $ liftM show $ readExpr "(cons '(this is) 'test)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "((this is) . test)"
    it "cons function works even with empty quoted lists" $ do
      evaled <- return $ liftM show $ readExpr "(cons '(this is) '())" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "((this is))"
    it "eqv? function works" $ do
      evaled <- return $ liftM show $ readExpr "(eqv? 1 3)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#f"
    it "eqv? function works" $ do
      evaled <- return $ liftM show $ readExpr "(eqv? 3 3)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#t"
    it "eqv? function works" $ do
      evaled <- return $ liftM show $ readExpr "(eqv? 'atom 'atom)" >>= eval
      str <- return $ extractValue $ trapError evaled
      str `shouldBe` "#t"