module Grid where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Node (Node(..), bomba, Coord)
import Node (Coord) 
type Grid = [[Node]]

-- Função que gera um campo minado aleatório e atualiza números adjacentes a bombas
generateGrid :: Int -> Int -> IO (Grid, Int)
generateGrid size numBombs = do
    -- Gera uma lista de posições para colocar bombas
    bombPositions <- selectBombPositions size numBombs
    initialGrid <- createGridWithBombs size bombPositions
    let gridWithCounts = countAdjacentBombs initialGrid size
    return (gridWithCounts, numBombs)
  where
    -- Seleciona posições aleatórias para as bombas
    selectBombPositions :: Int -> Int -> IO [Coord]
    selectBombPositions size n = do
        positions <- replicateM n $ do
            x <- randomRIO (0, size - 1)
            y <- randomRIO (0, size - 1)
            return (x, y)
        return (removeDuplicates positions)
    
    -- Cria o grid inicial com bombas nas posições selecionadas
    createGridWithBombs :: Int -> [Coord] -> IO Grid
    createGridWithBombs size bombPositions = 
        return [[if (i, j) `elem` bombPositions then Node bomba False else Node 0 False | j <- [0 .. size - 1]] | i <- [0 .. size - 1]]
    
    -- Remove duplicatas para garantir que cada posição tenha apenas uma bomba
    removeDuplicates :: [Coord] -> [Coord]
    removeDuplicates = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Conta o número de bombas perto de cada célula
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

-- Função para revelar bombas quando perde
revealBombs :: Grid -> Grid
revealBombs grid = [[revealNode node | node <- row] | row <- grid]
  where
    revealNode node@(Node d v)
        | d == bomba = Node bomba True -- A bomba é revelada
        | otherwise = node
