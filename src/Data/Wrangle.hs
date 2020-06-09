module Data.Wrangle ( DataFrame
                    , runPipe
                    , logState
                    , select
                    , filt
                    , names
                    , dims
                    ) where

import Control.Monad.State.Lazy
import qualified Data.Text as T
import Data.Bifunctor

type DataFrame = [(T.Text, [T.Text])]

logState :: (Show s) => StateT s IO ()
logState = do
  st <- get
  liftIO $ putStrLn $ show st

runPipe :: StateT DataFrame IO a -> IO a
runPipe pipe = evalStateT pipe []

select :: [T.Text] -> StateT DataFrame IO ()
select vars = modify $ selectByName vars

filt :: T.Text -> (T.Text -> Bool) -> StateT DataFrame IO ()
filt var f = do
  df <- get
  let selection = fmap f (snd . head $ selectByName [var] df)
      out = fmap (second (filtBool selection)) df
  put out

filtBool :: [Bool] -> [a] -> [a]
filtBool b xs = fmap snd $ filter fst (zip b xs)

selectByName :: Eq a => [a] -> [(a, b)] -> [(a, b)]
selectByName vars df = filter (flip elem vars . fst) df

names :: StateT DataFrame IO [T.Text]
names = get >>= return . fmap fst

dims :: StateT DataFrame IO [Int]
dims = do
  df <- get
  pure [length (snd (head df)), length df]
