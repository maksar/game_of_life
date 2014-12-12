import Data.HashSet
import Data.Hashable
import Control.Monad.Reader
import Data.Tuple.Extra

type Point = (Int, Int)

data State = Alive | Dead deriving (Eq, Show)

data Cell = Cell Point State deriving (Eq, Show)

alive :: Cell -> Bool
alive (Cell _ Alive) = True
alive (Cell _ Dead) = False

point :: Cell -> Point
point (Cell point _) = point

instance Hashable Cell where
  hashWithSalt salt (Cell (x, y) _) = salt `hashWithSalt` x `hashWithSalt` y

type Area = HashSet Cell

area :: Area
area = insert (Cell (1, 1) Alive) empty

type Neighbours = [Cell]

kill :: Cell -> Cell
kill (Cell point _) = Cell point Dead

born :: Cell -> Cell
born (Cell point _) = Cell point Alive

lifeCycle :: Cell -> Reader Neighbours Cell
lifeCycle cell = do
  count <- asks length
  return $ if alive cell then kill cell else Cell (both (+1) $ point cell) Alive

mapper :: Area -> Cell -> Cell
mapper area c = runReader (lifeCycle c) (toList $ area)

iteration :: Area -> Area
iteration a = Data.HashSet.map (mapper a) a

main :: IO ()
main = putStrLn $ show $ iterate iteration area !! 1000
