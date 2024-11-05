module Main where

import Grid (generateGrid, revealBombs, Grid)
import BFS (bfs)
import Node (Node(..), bomba, Coord)

-- Função teste
playGame :: Int -> IO ()
playGame size = do
    (grid, cntBombs) <- generateGrid size
    let win = size * size - cntBombs
    gameLoop grid size 0 win

gameLoop :: Grid -> Int -> Int -> Int -> IO ()
gameLoop grid size cnt win = do
    printGrid grid
    putStrLn "Enter your move \"rol col\" (1/n 1/n):"
    move <- getLine
    let [row, col] = map (\x -> read x - 1) (words move) :: [Int]
    (res, newCnt, newGrid) <- bfs grid size (row, col) cnt win
    if newCnt == win
       then do
           printGrid newGrid
           putStrLn "W"
       else if not res
               then do
                   let finalGrid = revealBombs newGrid -- Revela as bombas no grid final
                   printGrid finalGrid
                   putStrLn "L"
               else gameLoop newGrid size newCnt win

-- Imprime o grid
printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . unwords . map showNode)
  where
    showNode (Node d v)
        | not v     = "_"
        | d == bomba = "*"  -- Mostrar a bomba
        | otherwise = show d

-- Função main
main :: IO ()
main = do
    putStrLn "Enter the grid size:"
    size <- readLn :: IO Int
    playGame size
