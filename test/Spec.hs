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

{-
  describe "Data.Wrangle.filter" $ do
    it "can filter out rows" $ do
      let res = runPipe $ do
            put testData
            filt "a" (=="bar")
            dims
      res `shouldReturn` [1,2]
-}
  describe "Data.Wrangle.execute" $ do
    it "can make a calculation" $ do
      let res = runPipe $ do
            put testDataMut
            execute
      res `shouldReturn`
        [("a", ["abc", "def"]), ("f", ["abcfizz", "deffizz"])]


testData :: [Column]
testData = [("a", Left ["foo", "bar"]), ("b", Left ["foobar", "foobaz"])]

testDataGrp :: [Column]
testDataGrp = [("group", Left ["1","1","2","2"]), ("value", Left ["a","b","c","d"])]

testDataMut :: [Column]
testDataMut = [("a", Left ["abc", "def"]), ("f", Right $ Mutation ["a"] (\[x] -> fmap (<>"fizz") x))]
