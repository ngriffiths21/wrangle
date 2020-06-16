module Data.Wrangle ( Column
                    , Mutation (Mutation)
                    , Record (A,I,Grp)
                    , fromList
                    , grp
                    , mkFolder
                    , runPipe
                    , logState
                    , select
                    , names
                    , dims
                    , execute
                    ) where

import Control.Monad.State.Lazy
import qualified Data.Text as T
import GHC.Exts (groupWith)

type ColName = T.Text
data Record a = A | I | Grp Int a
  deriving (Show,Eq,Functor)

grp :: Record a -> Maybe Int
grp A = Nothing
grp I = Nothing
grp (Grp x _) = Just x

instance (Semigroup a) => Semigroup (Record a) where
  I <> _ = I
  _ <> I = I
  A <> _ = A
  _ <> A = A
  (Grp i x) <> (Grp j y)
    | i == j = Grp i (x <> y)
    | otherwise = I

fromList :: [T.Text] -> [Record T.Text]
fromList xs = fmap (Grp 1) xs

data Mutation = Mutation { cols :: [ColName]
                         , mut :: [[Record T.Text]] -> [Record T.Text]
                         }
  
instance Show Mutation where
  show _ = "Mutation"

type Column = (ColName, Either [Record T.Text] Mutation)

type ConcreteColumn = (ColName, [Record T.Text])

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

doMutate :: [Column] -> Mutation -> [Record T.Text]
doMutate df Mutation{cols, mut} = let
  concrete = fmap (runMutation df) cols 
  in applyMut mut (fmap snd concrete)
  
select :: [T.Text] -> StateT [Column] IO ()
select vars = modify $ selectByName vars

-- works with one-column mutations, unclear what happens when groups conflict
applyMut :: ([[Record T.Text]] -> [Record T.Text]) -> [[Record T.Text]] -> [Record T.Text]
applyMut fn x = let
  grouped = sequenceA $ fmap (groupWith grp) x
  in mconcat $ fmap fn grouped

mkFolder :: (Record a -> Record a -> Record a) -> [Record a] -> [Record a]
mkFolder fn l@(x:xs) = (foldl (fn) x xs) <$ l
mkFolder _ [] = []

selectByName :: Eq a => [a] -> [(a, b)] -> [(a, b)]
selectByName vars df = filter (flip elem vars . fst) df

names :: StateT [Column] IO [T.Text]
names = get >>= return . fmap fst

dims :: StateT [Column] IO [Int]
dims = do
  df <- get
  pure [length (snd (head df)), length df]

