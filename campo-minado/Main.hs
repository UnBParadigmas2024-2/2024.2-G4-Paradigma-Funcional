module Main where

import Grid (generateGrid, revealBombs, Grid)
import BFS (bfs)
import Node (Node(..), bomba, Coord)

-- Função para pedir o nível de dificuldade
askDifficulty :: IO String
askDifficulty = do
    putStrLn "Choose difficulty level (easy, normal, hard):"
    difficulty <- getLine
    return difficulty

-- Função para adaptar a geração de bombas com base na dificuldade
getBombChance :: String -> Int -> Int
getBombChance "easy" size = max 1 (size * size `div` 10)   
getBombChance "normal" size = max 2 (size * size `div` 5)  
getBombChance "hard" size = max 3 (size * size `div` 3)    -- quanto menor o valor, maior a dificuldade
getBombChance _ size = max 2 (size * size `div` 5)         


playGame :: Int -> String -> IO ()
playGame size difficulty = do
    let bombChance = getBombChance difficulty size  -- Passa o tamanho do grid
    (grid, cntBombs) <- generateGrid size bombChance  -- Gerar o grid com base no número de bombas
    let win = size * size - cntBombs  -- Condição de vitória

    -- Inicia o loop de jogo, passando o número de bombas
    gameLoop grid size 0 win cntBombs

gameLoop :: Grid -> Int -> Int -> Int -> Int -> IO ()
gameLoop grid size cnt win cntBombs = do
    -- Exibe o número de bombas antes de cada movimento
    putStrLn $ "Number of bombs: " ++ show cntBombs
    
    -- Exibe o grid
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
               else gameLoop newGrid size newCnt win cntBombs  -- Passa o número de bombas para a próxima chamada

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
    difficulty <- askDifficulty
    playGame size difficulty
