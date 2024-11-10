module BFS where

import Node (Node(..), bomba)
import Node (Coord)
import Grid (Grid, check, updateGrid, insertFlag, removeFlag)
import Control.Concurrent (threadDelay)

bfs :: Grid -> Int -> Coord -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
bfs grid size (i, j) cnt win flag = bfsRec grid [(i, j)] size cnt win flag

bfsRec :: Grid -> [Coord] -> Int -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
bfsRec grid [] _ cnt _ _ = return (True, cnt, grid)  -- Se a fila está vazia, acabou
bfsRec grid ((i, j):queue) size cnt win flag
    | flag && not (visited node) = do
        if not (hasFlag node) then do  -- Insere nova bandeira
            let gridWithNewFlag = insertFlag grid i j
            bfsRec gridWithNewFlag queue size cnt win True
        else do                        -- Remove a bandeira
            let gridWithOneLessFlag = removeFlag grid i j
            bfsRec gridWithOneLessFlag queue size cnt win False
    | not flag && (hasFlag node) = do  -- Caso clique onde há bandeira, não faz nada
        bfsRec grid queue size cnt win False
    | dataNode node == bomba = return (False, cnt, grid)  -- Bomba, perdeu
    | cnt == win = return (True, cnt, grid)  -- Se o contador atingir a quantidade total de células visitáveis, ganhou
    | visited node = bfsRec grid queue size cnt win False  -- Se o nó já foi visitado, segue pro prox
    | dataNode node /= 0 = do  -- Se for número, não visita mais nada
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
        threadDelay 5000
        bfsRec newGrid queue size newCnt win False
    | otherwise = do
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
        let expandedQueue = expandQueue newGrid size (i, j) queue
        bfsRec newGrid expandedQueue size newCnt win False
  where
    node = grid !! i !! j  -- Obtém o nó correspondente à coordenada (i, j)

expandQueue :: Grid -> Int -> Coord -> [Coord] -> [Coord]
expandQueue grid size (x, y) queue = foldl (\q (dx, dy) -> if check grid size (x + dx, y + dy) && not (visited (grid !! (x + dx) !! (y + dy))) then q ++ [(x + dx, y + dy)] else q) queue dxy
  where dxy = [(-1, 0), (0, 1), (1, 0), (0, -1)]

