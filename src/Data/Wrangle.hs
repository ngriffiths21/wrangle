module Data.Wrangle ( Column
                    , Mutation (Mutation)
                    , runPipe
                    , logState
                    , select
                    , filt
                    , names
                    , dims
                    , execute
                    ) where

import Control.Monad.State.Lazy
import qualified Data.Text as T
import Data.Bifunctor

type ColName = T.Text
type Record = [T.Text]

data Mutation = Mutation { cols :: [ColName]
                         , mut :: [Record] -> Record
                         }

instance Show Mutation where
  show x = "Mutation"

type Column = (ColName, Either Record Mutation)

type ConcreteColumn = (ColName, Record)

logState :: (Show s) => StateT s IO ()
logState = do
  st <- get
  liftIO $ putStrLn $ show st

runPipe :: StateT [Column] IO a -> IO a
runPipe pipe = evalStateT pipe []

execute :: StateT [Column] IO [ConcreteColumn]
execute = do
  df <- get
  let nms = fmap fst df
  pure $ fmap (runMutation df) nms

runMutation :: [Column] -> ColName -> ConcreteColumn
runMutation cols name = let
  loc = head $ selectByName [name] cols
  in case loc of
       (nm, Left r) -> (nm, r)
       (nm, (Right mt)) -> (nm, doMutate cols mt)

doMutate :: [Column] -> Mutation -> Record
doMutate df Mutation{cols, mut} = let
  concrete = fmap (runMutation df) cols 
  in mut (fmap snd concrete)
  
select :: [T.Text] -> StateT [Column] IO ()
select vars = modify $ selectByName vars

filt df f var = let
  selection = fmap f (snd . head $ selectByName [var] df)
  out = fmap (second (filtBool selection)) df
  in out


filtBool :: [Bool] -> [a] -> [a]
filtBool b xs = fmap snd $ filter fst (zip b xs)

selectByName :: Eq a => [a] -> [(a, b)] -> [(a, b)]
selectByName vars df = filter (flip elem vars . fst) df

{-
calc :: (T.Text -> T.Text) -> ([T.Text] -> [T.Text]) -> [T.Text] -> StateT [Column] IO ()
calc namef valf cols = do
  df <- get
  let old = selectByName cols df
      trans (nms, vals) = (namef nms, valf vals)
      out = fmap trans old
  put (df ++ out)
-}

names :: StateT [Column] IO [T.Text]
names = get >>= return . fmap fst

dims :: StateT [Column] IO [Int]
dims = do
  df <- get
  pure [length (snd (head df)), length df]
