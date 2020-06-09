import Test.Hspec
import Data.Wrangle as W
import Control.Monad.State.Lazy

main :: IO ()
main = hspec $ do
  describe "Data.Wrangle.runPipe" $ do
    it "can run an empty pipeline" $ do
      W.runPipe $ pure ()

  describe "Data.Wrangle.log" $ do
    it "can log the current state" $ do
      W.runPipe W.logState

  describe "Data.Wrangle.select" $ do
    it "can select some variables" $ do
      res `shouldReturn` ["a"] where
        res = W.runPipe $ do
          put testData
          select ["a"]
          names

testData :: W.DataFrame
testData = [("a", ["a value 1"]), ("b", ["b value 1"])]
