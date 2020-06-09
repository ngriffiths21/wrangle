module Data.Wrangle ( DataFrame
                    , runPipe
                    , logState
                    , select
                    , names
                    ) where

import Control.Monad.State.Lazy
import qualified Data.Text as T

type DataFrame = [(T.Text, [T.Text])]

logState :: (Show s) => StateT s IO ()
logState = do
  st <- get
  liftIO $ putStrLn $ show st

runPipe :: StateT DataFrame IO a -> IO a
runPipe pipe = evalStateT pipe []

select :: [T.Text] -> StateT DataFrame IO ()
select vars = do
  df <- get
  put $ filter (flip elem vars . fst) df

names :: StateT DataFrame IO [T.Text]
names = get >>= return . fmap fst
