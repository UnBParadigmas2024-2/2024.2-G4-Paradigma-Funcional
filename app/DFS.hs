module DFS where

import Node (Node(..), bomba)
import Node (Coord)
import Grid (Grid, check, updateGrid, insertFlag, removeFlag)


-- Função principal DFS
dfs :: Grid -> Int -> Coord -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
dfs grid size (i, j) cnt win flag = dfsRec grid [(i, j)] size cnt win flag

-- Função recursiva para o DFS
dfsRec :: Grid -> [Coord] -> Int -> Int -> Int -> Bool -> IO (Bool, Int, Grid)
dfsRec grid [] _ cnt _ _ = return (True, cnt, grid)  -- Se a pilha está vazia, acabou
dfsRec grid ((i, j):stack) size cnt win flag
    | flag && not (visited node) = do
        if not (hasFlag node) then do  -- Insere nova bandeira
            let gridWithNewFlag = insertFlag grid i j
            dfsRec gridWithNewFlag stack size cnt win True
        else do                        -- Remove a bandeira
            let gridWithOneLessFlag = removeFlag grid i j
            dfsRec gridWithOneLessFlag stack size cnt win False
    | not flag && (hasFlag node) = do  -- Caso clique onde há bandeira, não faz nada
        dfsRec grid stack size cnt win False
    | dataNode node == bomba = return (False, cnt, grid)  -- Bomba, perdeu
    | cnt == win = return (True, cnt, grid)  -- Se o contador atingir a quantidade total de células visitáveis, ganhou
    | visited node = dfsRec grid stack size cnt win False  -- Se o nó já foi visitado, segue para o próximo
    | dataNode node /= 0 = do  -- Se for número, não visita mais nada
        let newGrid = updateGrid grid i j
            newCnt = if not (visited node) then cnt + 1 else cnt
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

