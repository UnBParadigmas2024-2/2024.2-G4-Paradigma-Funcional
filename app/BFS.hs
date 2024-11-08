module BFS where

import Node (Node(..), bomba)
import Node (Coord)
import Grid (Grid, check, updateGrid, showFlag)

bfs :: Grid -> Int -> Coord -> Int -> Int -> Int -> IO (Bool, Int, Grid)
bfs grid size (i, j) cnt win flag = bfsRec grid [(i, j)] flag size cnt win

bfsRec :: Grid -> [Coord] -> Int -> Int -> Int -> Int -> IO (Bool, Int, Grid)
bfsRec grid [] _ _ cnt _ = return (True, cnt, grid)  -- Se a fila está vazia, acabou
bfsRec grid ((i, j):queue) flag size cnt win
    | flag == 1 && not (visited node) = do -- Verifica se onde o usuário quer por a bandeira já foi visitado
        let gridWithFlag = showFlag grid i j
        bfsRec gridWithFlag queue 1 size cnt win
    | dataNode node == bomba = return (False, cnt, grid)  -- Bomba, perdeu
    | cnt == win = return (True, cnt, grid)  -- Se o contador atingir a quantidade total de células visitáveis, ganhou
    | visited node = bfsRec grid queue 0 size cnt win  -- Se o nó já foi visitado, segue pro prox
    | dataNode node /= 0 = do  -- Se for número, não visita mais nada
        let newGrid = updateGrid grid i j
            newCnt
                | not (visited node) = cnt + 1
                | otherwise = cnt
        bfsRec newGrid queue 0 size newCnt win
    | otherwise = do
        let newGrid = updateGrid grid i j
            newCnt
                | not (visited node) = cnt + 1
                | otherwise = cnt
        let expandedQueue = expandQueue newGrid size (i, j) queue
        bfsRec newGrid expandedQueue 0 size newCnt win
  where
    node = grid !! i !! j  -- Obtém o nó correspondente à coordenada (i, j)

expandQueue :: Grid -> Int -> Coord -> [Coord] -> [Coord]
expandQueue grid size (x, y) queue = foldl (\q (dx, dy) -> if check grid size (x + dx, y + dy) && not (visited (grid !! (x + dx) !! (y + dy))) then q ++ [(x + dx, y + dy)] else q) queue dxy
  where dxy = [(-1, 0), (0, 1), (1, 0), (0, -1)]
