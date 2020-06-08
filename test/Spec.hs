import Test.Hspec
import Data.Wrangle as W
-- import Control.Monad.State.Lazy

main :: IO ()
main = hspec $ do
  describe "Data.Wrangle.runPipe" $ do
    it "can run an empty pipeline" $ do
      W.runPipe $ pure ()

  describe "Data.Wrangle.log" $ do
    it "can log the current state" $ do
      W.runPipe W.logState
