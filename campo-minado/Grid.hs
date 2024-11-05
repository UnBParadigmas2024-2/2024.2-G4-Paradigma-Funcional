module Grid where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Node (Node(..), bomba, Coord)
import Node (Coord) 
type Grid = [[Node]]

-- Função que gera um campo minado aleatório e atualiza números adjacentes a bombas
generateGrid :: Int -> IO (Grid, Int)
generateGrid size = do
    initialGrid <- replicateM size (replicateM size randomNode)
    let gridWithCounts = countAdjacentBombs initialGrid size
        countBombs = length $ filter (\n -> dataNode n == bomba) (concat initialGrid)
    return (gridWithCounts, countBombs)
  where
    randomNode :: IO Node
    randomNode = do
        rand <- randomRIO (0 :: Int, 4 :: Int) -- 20% de chance de ser uma bomba, pode mudar com alguma dificuldade, sei la
        return $ Node (if rand == 0 then bomba else 0) False

-- Conta o número de bombas perto de cada celula
countAdjacentBombs :: Grid -> Int -> Grid
countAdjacentBombs grid size = [[updateNode i j | j <- [0 .. size - 1]] | i <- [0 .. size - 1]]
  where
    updateNode i j
        | dataNode (grid !! i !! j) == bomba = Node bomba False
        | otherwise = Node (countBombs i j) False

    countBombs i j = length [() | (dx, dy) <- dxy, isBomb (i + dx) (j + dy)]
    isBomb x y = x >= 0 && x < size && y >= 0 && y < size && dataNode (grid !! x !! y) == bomba

    dxy = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

-- Verifica se uma coordenada é válida e não é uma bomba
check :: Grid -> Int -> Coord -> Bool
check grid size (i, j) =
    i >= 0 && i < size && j >= 0 && j < size &&
    dataNode (grid !! i !! j) /= bomba &&
    not (visited (grid !! i !! j))

-- Atualiza o grid para marcar um nó como visitado
updateGrid :: Grid -> Int -> Int -> Grid
updateGrid grid i j = 
    let (ys, zs:zs') = splitAt i grid
        (xs, node:xs') = splitAt j zs
        newNode = node { visited = True }
    in ys ++ ((xs ++ (newNode : xs')) : zs')

-- Função para revelar bombas qd perde
revealBombs :: Grid -> Grid
revealBombs grid = [[revealNode node | node <- row] | row <- grid]
  where
    revealNode node@(Node d v)
        | d == bomba = Node bomba True -- A bomba é revelada
        | otherwise = node
