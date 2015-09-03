import Cap_9_module.Complete_code
import Test.Hspec

-- Recuerda: escapar todo lo que pueda dar problemas en las strings!

main = hspec $ do
  describe "validate" $ do
    it "the case structure works" $ do
      result <- runOne ["case1.scm"]
      result `shouldBe` "jorgito"
    it "the factorial program works" $ do
      result <- runOne ["fact.scm"]
      result `shouldBe` "3628800"
    it "the Fibonacci program works" $ do
      result <- runOne ["fib.scm"]
      result `shouldBe` "13"
    it "the greater common divisor program works" $ do
      result <- runOne ["gcd.scm"]
      result `shouldBe` "18"
    it "lambda expressions works" $ do
      result <- runOne ["lambda_7.scm"]
      result `shouldBe` "7"
    it "lambda expressions works works" $ do
      result <- runOne ["lambda_9.scm"]
      result `shouldBe` "9"