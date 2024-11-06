module Node where

type Coord = (Int, Int)
data Node = Node { dataNode :: Int, visited :: Bool } deriving Show

bomba :: Int
bomba = -1
