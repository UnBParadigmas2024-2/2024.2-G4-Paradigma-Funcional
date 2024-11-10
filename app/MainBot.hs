module Main where

-- Importa os módulos necessários
import Game (GameState(..), gameInit, gameUpdate)
import Bot (playBot)
import Control.Monad (when)
import Node (Node(..), bomba)

gridSize :: Int
gridSize = 8  -- Tamanho da grade

difficulty :: String
difficulty = "easy"  -- Nível de dificuldade

structure :: String
structure = "bfs"

-- Função para imprimir o tabuleiro
printCustomGrid :: [[Node]] -> IO ()
printCustomGrid = mapM_ (putStrLn . unwords . map showCustomNode)
  where
    -- Exibe cada célula do tabuleiro
    showCustomNode (Node d v f)
        | not v && f = "!"           -- Célula não revelada e com bandeira
        | not v && d == bomba = "#"  -- Célula não revelada e com bomba
        | not v = "_"               -- Célula não revelada
        | d == bomba = "#"          -- Célula com bomba
        | otherwise = show d        -- Célula revelada

main :: IO ()
main = do
    -- Mensagens iniciais
    putStrLn "Comecando Minesweeper Bot..."
    putStrLn $ "Grid Size: " ++ show gridSize ++ "x" ++ show gridSize
    putStrLn $ "Dificuladade: " ++ difficulty
    putStrLn "Grid Initial:"
    
    -- Inicializa o jogo
    state <- gameInit gridSize difficulty structure
    printCustomGrid (state'grid state)
    
    -- Jogo com o bot
    finalState <- playBotGame state
    
    -- Resultado final
    putStrLn $ if state'lose finalState 
               then "Bot Perdeu!"  
               else if state'finished finalState 
                    then "Bot Ganhou!"  
                    else "Game incompleto!" 

-- Função recursiva que faz o bot jogar até o fim
playBotGame :: GameState -> IO GameState
playBotGame state
    | state'finished state = return state  -- Jogo finalizado
    | otherwise = do
        newState <- playBot state
        putStrLn "\nGrid after bot's move:"
        printCustomGrid (state'grid newState)
        playBotGame newState
