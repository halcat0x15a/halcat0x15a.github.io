import Test.Hspec
import Deriving.Lens
import Deriving.Person

main :: IO ()
main = hspec $ do
  describe "Deriving Lens in Haskell" $ do
    it "identity" $ do
      (get personAge . set personAge 22 $ Person "Sanshiro Yoshida" 21) `shouldBe` 22
