module Data.Wrangle ( runPipe
                    , logState
                    ) where

import Control.Monad.State.Lazy

type Data = [String]

logState :: (Show s) => StateT s IO ()
logState = do
  st <- get
  liftIO $ putStrLn $ show st

runPipe :: StateT Data IO a -> IO a
runPipe pipe = evalStateT pipe []
