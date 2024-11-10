module Bot where

import Node (Node(..), bomba, Coord)
import Grid (Grid)
import Game (GameState(..), gameUpdate)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Random (randomRIO)

-- Tipo de dado que armazena o conhecimento sobre as células
data CellKnowledge = CellKnowledge {
    isBomb :: Bool,       -- Se a célula contém uma bomba
    probability :: Float, -- Probabilidade de ser uma bomba
    isRevealed :: Bool   -- Se a célula foi revelada
} deriving Show

-- Inicializa o conhecimento das células com valores padrão
initKnowledge :: Int -> [[CellKnowledge]]
initKnowledge size = replicate size $ replicate size $ CellKnowledge False 0.5 False

-- Atualiza o conhecimento com base nas células reveladas
updateKnowledge :: Grid -> [[CellKnowledge]] -> [[CellKnowledge]]
updateKnowledge grid knowledge = 
    [[updateCell i j | j <- [0..length grid - 1]] | i <- [0..length grid - 1]]
  where
    -- Atualiza uma célula individual
    updateCell i j
        | visited (grid !! i !! j) = CellKnowledge False 0.0 True  -- Célula revelada
        | hasFlag (grid !! i !! j) = CellKnowledge True 1.0 False  -- Célula com bandeira
        | otherwise = calculateProbability i j  -- Caso contrário, calcula a probabilidade de ser bomba

    -- Calcula a probabilidade de uma célula ter bomba
    calculateProbability i j = 
        let adjacentCells = getAdjacentCells grid (i, j)
            bombProb = calculateBombProbability grid knowledge (i, j)
        in CellKnowledge False bombProb False

-- Calcula a probabilidade de uma célula ser bomba
calculateBombProbability :: Grid -> [[CellKnowledge]] -> Coord -> Float
calculateBombProbability grid knowledge (i, j) =
    let adjacent = getAdjacentCells grid (i, j)
        revealedAdjacent = filter (\(x, y) -> visited (grid !! x !! y)) adjacent
        totalBombs = sum [fromIntegral $ dataNode (grid !! x !! y) | (x, y) <- revealedAdjacent]
        unrevealed = length adjacent - length revealedAdjacent
    in if unrevealed == 0 then 0.0 else min 1.0 (fromIntegral totalBombs / fromIntegral unrevealed)

-- Obtém as células adjacentes válidas
getAdjacentCells :: Grid -> Coord -> [Coord]
getAdjacentCells grid (i, j) = 
    [(x, y) | x <- [i-1..i+1], 
              y <- [j-1..j+1],
              x >= 0, x < length grid,
              y >= 0, y < length grid,
              (x, y) /= (i, j)]  -- Exclui a célula (i, j)

-- Escolhe o próximo movimento do bot baseado no conhecimento
chooseMove :: GameState -> [[CellKnowledge]] -> IO (Int, Int, Bool)
chooseMove state knowledge = do
    let size = state'size state
    let grid = state'grid state
    let safeMoves = [(i, j) | i <- [0..size-1], 
                             j <- [0..size-1],
                             not (visited (grid !! i !! j)), 
                             not (hasFlag (grid !! i !! j)),
                             probability (knowledge !! i !! j) < 0.3]  -- Células seguras

    if null safeMoves
        then do
            -- Se não houver movimentos seguros, escolhe a célula com menor probabilidade de bomba
            let unrevealedCells = [(i, j) | i <- [0..size-1], j <- [0..size-1], 
                                           not (visited (grid !! i !! j)), 
                                           not (hasFlag (grid !! i !! j))]
            if null unrevealedCells
                then return (0, 0, False)  -- Se não houver mais células, retorna (0, 0)
                else do
                    let (i, j) = minimumBy (comparing (\(x, y) -> probability (knowledge !! x !! y))) unrevealedCells
                    return (i, j, False)
        else do
            -- Escolhe aleatoriamente um movimento seguro
            index <- randomRIO (0, length safeMoves - 1)
            let (i, j) = safeMoves !! index
            return (i, j, False)

-- Função principal que faz o bot jogar
playBot :: GameState -> IO GameState
playBot state = do
    let knowledge = initKnowledge (state'size state)  -- Inicializa o conhecimento
    playBotTurn state knowledge  -- Executa o turno do bot

-- Função para jogar um turno do bot
playBotTurn :: GameState -> [[CellKnowledge]] -> IO GameState
playBotTurn state knowledge = do
    if state'finished state  -- Se o jogo acabou, retorna o estado atual
        then return state
        else do
            putStrLn "\nTurno do bot..."
            let updatedKnowledge = updateKnowledge (state'grid state) knowledge  -- Atualiza o conhecimento
            (row, col, isFlag) <- chooseMove state updatedKnowledge  -- Escolhe o movimento
            putStrLn $ "Bot move: (" ++ show (row + 1) ++ ", " ++ show (col + 1) ++ ")"
            newState <- gameUpdate state row col isFlag  -- Atualiza o estado do jogo
            playBotTurn newState updatedKnowledge  -- Chama recursivamente para o próximo turno
