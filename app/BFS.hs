module BFS where

import Node (Node(..), bomba)
import Node (Coord)
import Grid (Grid, check, updateGrid, showFlag)
import Control.Concurrent (threadDelay)

bfs :: Grid -> Int -> Coord -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
bfs grid size (i, j) cnt win flag = bfsRec grid [(i, j)] size cnt win flag

bfsRec :: Grid -> [Coord] -> Int -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
bfsRec grid [] _ cnt _ _ = return (True, cnt, grid)  -- Se a fila está vazia, acabou
bfsRec grid ((i, j):queue) size cnt win flag = do
    if flag && not (visited node)
    then do
        let gridWithFlag = showFlag grid i j
        bfsRec gridWithFlag queue size cnt win True
    else if dataNode node == bomba
    then return (False, cnt, grid)  -- Bomba, perdeu
    else if cnt == win
    then return (True, cnt, grid)  -- Se o contador atingir a quantidade total de células visitáveis, ganhou
    else if visited node
    then bfsRec grid queue size cnt win False  -- Se o nó já foi visitado, segue pro prox
    else if dataNode node /= 0
    then do  -- Se for número, não visita mais nada
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
        threadDelay 5000
        bfsRec newGrid queue size newCnt win False
    else do
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
        let expandedQueue = expandQueue newGrid size (i, j) queue
        bfsRec newGrid expandedQueue size newCnt win False
  where
    node = grid !! i !! j  -- Obtém o nó correspondente à coordenada (i, j)

expandQueue :: Grid -> Int -> Coord -> [Coord] -> [Coord]
expandQueue grid size (x, y) queue = foldl (\q (dx, dy) -> if check grid size (x + dx, y + dy) && not (visited (grid !! (x + dx) !! (y + dy))) then q ++ [(x + dx, y + dy)] else q) queue dxy
  where dxy = [(-1, 0), (0, 1), (1, 0), (0, -1)]



-- Função principal DFS
dfs :: Grid -> Int -> Coord -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
dfs grid size (i, j) cnt win flag = dfsRec grid [(i, j)] size cnt win flag

-- Função recursiva para o DFS
dfsRec :: Grid -> [Coord] -> Int -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
dfsRec grid [] _ cnt _ _ = return (True, cnt, grid)  -- Se a pilha está vazia, acabou
dfsRec grid ((i, j):stack) size cnt win flag
    | flag == True && not (visited node) = do
        let gridWithFlag = showFlag grid i j
        dfsRec gridWithFlag stack size cnt win True
    | dataNode node == bomba = return (False, cnt, grid)  -- Bomba, perdeu
    | cnt == win = return (True, cnt, grid)  -- Se o contador atingir a quantidade total de células visitáveis, ganhou
    | visited node = dfsRec grid stack size cnt win False  -- Se o nó já foi visitado, segue para o próximo
    | dataNode node /= 0 = do  -- Se for número, não visita mais nada
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
        threadDelay 5000
        dfsRec newGrid stack size newCnt win False
    | otherwise = do
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
        let expandedStack = expandStack newGrid size (i, j) stack
        dfsRec newGrid expandedStack size newCnt win False
  where
    node = grid !! i !! j  -- Obtém o nó correspondente à coordenada (i, j)

-- Expande a pilha (LIFO)
expandStack :: Grid -> Int -> Coord -> [Coord] -> [Coord]
expandStack grid size (x, y) stack = 
    foldr (\(dx, dy) acc -> 
        let newCoord = (x + dx, y + dy)
        in if check grid size newCoord && not (visited (grid !! (x + dx) !! (y + dy)))
           then newCoord : acc
           else acc
    ) stack dxy
  where
    dxy = [(-1, 0), (0, 1), (1, 0), (0, -1)]  -- Movimentos para expandir (cima, direita, baixo, esquerda)