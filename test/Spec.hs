import Test.Hspec
import Data.Wrangle
import Control.Monad.State.Lazy

main :: IO ()
main = hspec $ do
  describe "Data.Wrangle.runPipe" $ do
    it "can run an empty pipeline" $ do
      runPipe $ pure ()

  describe "Data.Wrangle.log" $ do
    it "can log the current state" $ do
      runPipe logState

  describe "Data.Wrangle.select" $ do
    it "can select some variables" $ do
        let res = runPipe $ do
                put testData
                select ["a"]
                names
        res `shouldReturn` ["a"]

  describe "Data.Wrangle.filter" $ do
    it "can filter out rows" $ do
      let res = runPipe $ do
            put testData
            df <- get
            filt "a" (=="bar")
            dims
      res `shouldReturn` [1,2]

testData :: DataFrame
testData = [("a", ["foo", "bar"]), ("b", ["foobar", "foobaz"])]
