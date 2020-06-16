import Test.Hspec
import Data.Wrangle
import Control.Monad.State.Lazy
import GHC.Exts (groupWith)

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
        [("a", fromList ["abc", "def"]), ("f", fromList ["abcfizz", "deffizz"])]

  describe "Data.Wrangle.execute" $ do
    it "can do a grouped calculation" $ do
      let res = runPipe $ do
            put testDataGrp
            execute
      res `shouldReturn`
        [("a", [Grp 1 "a", Grp 1 "b", Grp 2 "c", Grp 2 "d"]),
         ("f", [Grp 1 "ab", Grp 1 "ab", Grp 2 "cd", Grp 2 "cd"])]

testData :: [Column]
testData = [("a", Left $ fromList ["foo", "bar"]), ("b", Left $ fromList ["foobar", "foobaz"])]

testDataMut :: [Column]
testDataMut = [("a", Left (fromList ["abc", "def"])), ("f", Right $ Mutation ["a"] (\[x] -> fmap (fmap (<>"fizz")) x))]

testDataGrp :: [Column]
testDataGrp = [("a", Left [Grp 1 "a", Grp 1 "b", Grp 2 "c", Grp 2 "d"]),
               ("f", Right $ Mutation ["a"] (\[x] -> mkFolder (<>) x))]
