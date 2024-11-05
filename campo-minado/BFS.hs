module BFS where

import Node (Node(..), bomba)
import Node (Coord) 
import Grid (Grid, check, updateGrid)
-- Busca em largura recursiva no campo minado
bfs :: Grid -> Int -> Coord -> Int -> Int -> IO (Bool, Int, Grid)
bfs grid size (i, j) cnt win
    | dataNode (grid !! i !! j) == bomba = return (False, cnt, grid) -- Morreu
    | cnt == win                         = return (True, cnt, grid)  -- Vitória
    | otherwise                          = do
        let newGrid = updateGrid grid i j
            newCnt = if not (visited (grid !! i !! j)) then cnt + 1 else cnt
        -- Expande apenas se o no n tiver numero ou bomba
        if dataNode (grid !! i !! j) == 0
           then explore newGrid size [(i + dx, j + dy) | (dx, dy) <- dxy] newCnt win
           else return (True, newCnt, newGrid)
  where
    dxy = [(-1, 0), (0, 1), (1, 0), (0, -1)]

    explore g sz [] c w = return (True, c, g)
    explore g sz (p:ps) c w
        | check g sz p = do
            (res, nc, ng) <- bfs g sz p c w
            if res then explore ng sz ps nc w else return (False, nc, ng)
        | otherwise    = explore g sz ps c w
