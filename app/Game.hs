module Game 
    ( GameState(..)  -- Exporta o tipo de dado e seus construtores
    , gameInit
    , gameUpdate
    , gameRestart
    , state'grid
    , state'size
    , state'cnt
    , state'finished
    , state'win
    , state'lose
    , state'remainingBombs
    , state'difficulty
    , state'startTime    -- Novo campo exportado
    , state'currentTime  -- Novo campo exportado
    , state'structureType
    , playGameBFS
    , printGrid
    , cellHasFlag
    ) where

import Grid (generateGrid, revealBombs, Grid)
import BFS (bfs)
import DFS (dfs)
import Node (Node(..), bomba)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

data GameState = GameState {
    state'grid     :: Grid,
    state'size     :: Int,
    state'cnt      :: Int,
    state'win      :: Int,
    state'remainingBombs :: Int, 
    state'remainingFlags :: Int, 
    state'finished :: Bool,
    state'lose     :: Bool,
    state'difficulty :: String,
    state'startTime :: UTCTime,  -- Novo campo para armazenar o tempo inicial
    state'currentTime :: Int,     -- Novo campo para armazenar o tempo decorrido em segundos
    state'structureType :: String
}

-- Função para adaptar a geração de bombas com base na dificuldade
getBombChance :: String -> Int -> Int
getBombChance "easy" size = max 1 (size * size `div` 10)   
getBombChance "normal" size = max 2 (size * size `div` 5)  
getBombChance "hard" size = max 3 (size * size `div` 3)    -- quanto menor o valor, maior a dificuldade
getBombChance _ size = max 2 (size * size `div` 5)         

gameRestart :: GameState -> IO GameState
gameRestart state = do
    startTime <- getCurrentTime
    newState <- gameInit (state'size state) (state'difficulty state) (state'structureType state)
    return newState

gameInit :: Int -> String -> String -> IO GameState
gameInit size difficulty structure = do
    let bombChance = getBombChance difficulty size
    (grid, cntBombs) <- generateGrid size bombChance
    let win = size * size - cntBombs
    let remainingFlags = cntBombs
    startTime <- getCurrentTime
    let structureType = structure
    
    return (GameState grid size 0 win cntBombs remainingFlags False False difficulty startTime 0 structureType)

gameUpdate :: GameState -> Int -> Int -> Bool -> IO (GameState)
gameUpdate state row col isSettingFlag = do
    if (state'structureType state == "bfs")
        then do
            (res, newCnt, newGrid) <- bfs (state'grid state) (state'size state) (row, col) (state'cnt state) (state'win state) isSettingFlag
            let hasWon = (state'win state) == newCnt
            let hasLost = not hasWon && not res
            let finished = hasLost || hasWon

            let updatedRemainingFlags = 
                    if isSettingFlag then 
                        if (cellHasFlag (state'grid state) row col) then
                            -- Remover bandeira (caso já tenha uma bandeira)
                            min (state'remainingFlags state + 1) (state'remainingBombs state)
                        else
                            -- Adicionar bandeira (caso não tenha uma bandeira)
                            max (state'remainingFlags state - 1) 0
                    else
                        state'remainingFlags state

            return $ GameState 
                newGrid 
                (state'size state) 
                newCnt 
                (state'win state)
                (state'remainingBombs state) 
                updatedRemainingFlags
                finished 
                hasLost
                (state'difficulty state)
                (state'startTime state)
                (state'currentTime state)
                (state'structureType state)
        else do
            (res, newCnt, newGrid) <- dfs (state'grid state) (state'size state) (row, col) (state'cnt state) (state'win state) isSettingFlag
            let hasWon = (state'win state) == newCnt
            let hasLost = not hasWon && not res
            let finished = hasLost || hasWon

            let updatedRemainingFlags = 
                    if isSettingFlag then 
                        if (cellHasFlag (state'grid state) row col) then
                            -- Remover bandeira (caso já tenha uma bandeira)
                            min (state'remainingFlags state + 1) (state'remainingBombs state)
                        else
                            -- Adicionar bandeira (caso não tenha uma bandeira)
                            max (state'remainingFlags state - 1) 0
                    else
                        state'remainingFlags state

            return $ GameState 
                newGrid 
                (state'size state) 
                newCnt 
                (state'win state)
                (state'remainingBombs state) 
                updatedRemainingFlags
                finished 
                hasLost
                (state'difficulty state)
                (state'startTime state)
                (state'currentTime state)
                (state'structureType state)

-- Função para verificar se uma célula já possui uma bandeira
cellHasFlag :: [[Node]] -> Int -> Int -> Bool 
cellHasFlag grid row col = 
    hasFlag (grid !! row !! col)

playGameBFS :: Int -> String -> IO ()
playGameBFS size difficulty = do
    let bombChance = getBombChance difficulty size  -- Passa o tamanho do grid
    (grid, cntBombs) <- generateGrid size bombChance  -- Gerar o grid com base no número de bombas
    let win = size * size - cntBombs  -- Condição de vitória

    -- Inicia o loop de jogo, passando o número de bombas
    gameLoopBFS grid size 0 win cntBombs

gameLoopBFS :: Grid -> Int -> Int -> Int -> Int -> IO ()
gameLoopBFS grid size cnt win cntBombs = do
    -- Exibe o número de bombas antes de cada movimento
    putStrLn $ "Number of bombs: " ++ show cntBombs
    printGrid grid
    putStrLn "Enter your move \"row col\" or set a flag \"row col 1\":"
    move <- getLine
    let userInput = words move
    let [row, col] = map (\x -> read x - 1) (take 2 userInput) :: [Int] 
    let initialQueue = [(row, col)]  -- A fila começa com a coordenada inicial
    
    let flag = if length userInput <= 2 then False else True

    (res, newCnt, newGrid) <- bfs grid size (row, col) cnt win flag
    gameLoopBFS' newCnt newGrid res cntBombs

  where
    gameLoopBFS' newCnt newGrid res cntBombs
      | newCnt == win = do
          printGrid newGrid
          putStrLn "You win!"
      | not res = do
          let finalGrid = revealBombs newGrid  -- Revela as bombas no grid final
          printGrid finalGrid
          putStrLn "You lose!"
      | otherwise = gameLoopBFS newGrid size newCnt win cntBombs  -- Continua o jogo com o novo grid e contador

-- Imprime o grid
printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . unwords . map showNode)
  where
    showNode (Node d v f)
        | not v && f == False = "_"
        | not v && f == True = "!" -- Sinalização de possível bomba
        | d == bomba = "*"  -- Mostrar a bomba
        | otherwise = show d

