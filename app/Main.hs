{-# LANGUAGE PatternSynonyms #-}
module Main where


-- Assets do campo minado
-- https://github.com/BrandonDusseau/minesweeper-classic

import Control.Monad (forM_, foldM)
import Paths_CampoMinado (getDataFileName)
import Raylib.Core (clearBackground, isMouseButtonPressed, getMousePosition, getMouseX, getMouseY)
import Raylib.Core.Textures
  ( 
    drawTexturePro,
    loadImage,
    loadTextureFromImage,
  )
import Raylib.Types (Rectangle (Rectangle, rectangle'height, rectangle'width), pattern Vector2, MouseButton (..), Font (font'baseSize))
import Raylib.Util (drawing, whileWindowOpen_, withWindow, managed)
import Raylib.Util.Colors (black, white)

import Game (gameInit, gameUpdate, state'grid, state'cnt, state'finished, state'win, state'lose, state'remainingBombs, printGrid)
import Node (Node(..), bomba)
import Grid (Grid)
import GHC.Generics (S)
import Raylib.Core.Text (drawText)

spriteError      :: Rectangle; spriteError      = (Rectangle (96)   (64) 16 16) -- Área hachurada em rosa
spriteVisited    :: Rectangle; spriteVisited    = (Rectangle (32) (83) 16 16)
spriteNotVisited :: Rectangle; spriteNotVisited = (Rectangle (50) (83) 16 16)
spriteBomb       :: Rectangle; spriteBomb       = (Rectangle (16*2) (21+16*2) 16 16)
spriteBombCount0 :: Rectangle; spriteBombCount0 = (Rectangle (32)   (84) 16 16) -- É apenas um quadrado transparente no .gif
spriteBombCount1 :: Rectangle; spriteBombCount1 = (Rectangle (16*0) (21+0) 16 16)
spriteBombCount2 :: Rectangle; spriteBombCount2 = (Rectangle (16*1) (21+0) 16 16)
spriteBombCount3 :: Rectangle; spriteBombCount3 = (Rectangle (16*2) (21+0) 16 16)
spriteBombCount4 :: Rectangle; spriteBombCount4 = (Rectangle (16*3) (21+0) 16 16)
spriteBombCount5 :: Rectangle; spriteBombCount5 = (Rectangle (16*0) (21+16) 16 16)
spriteBombCount6 :: Rectangle; spriteBombCount6 = (Rectangle (16*1) (21+16) 16 16)
spriteBombCount7 :: Rectangle; spriteBombCount7 = (Rectangle (16*2) (21+16) 16 16)
spriteBombCount8 :: Rectangle; spriteBombCount8 = (Rectangle (16*3) (21+16) 16 16)
spriteFlag       :: Rectangle; spriteFlag       = (Rectangle (16*1) (21+16*2) 16 16)

getRectForVisibleCellSprite :: Grid -> Int -> Int -> Rectangle
getRectForVisibleCellSprite grid row col = rect
  where 
    isVisited   = visited (grid !! row !! col)
    bombsAround = dataNode (grid !! row !! col)
    flagSet = hasFlag (grid !! row !! col)
    rect
      | isVisited && bombsAround == bomba = spriteBomb
      | isVisited && bombsAround == 0 = spriteVisited
      | isVisited && bombsAround == 1 = spriteBombCount1
      | isVisited && bombsAround == 2 = spriteBombCount2
      | isVisited && bombsAround == 3 = spriteBombCount3
      | isVisited && bombsAround == 4 = spriteBombCount4
      | isVisited && bombsAround == 5 = spriteBombCount5
      | isVisited && bombsAround == 6 = spriteBombCount6
      | isVisited && bombsAround == 7 = spriteBombCount7
      | isVisited && bombsAround == 8 = spriteBombCount8
      | (not isVisited) && flagSet = spriteFlag
      | not isVisited = spriteNotVisited
      | otherwise     = spriteError


getRectForHiddenCellSprite :: Grid -> Int -> Int -> Rectangle
getRectForHiddenCellSprite grid row col = rect
  where
    bombsAround = dataNode (grid !! row !! col)
    rect
      | bombsAround == bomba = spriteBomb
      | bombsAround == 0 = spriteVisited
      | bombsAround == 1 = spriteBombCount1
      | bombsAround == 2 = spriteBombCount2
      | bombsAround == 3 = spriteBombCount3
      | bombsAround == 4 = spriteBombCount4
      | bombsAround == 5 = spriteBombCount5
      | bombsAround == 6 = spriteBombCount6
      | bombsAround == 7 = spriteBombCount7
      | bombsAround == 8 = spriteBombCount8
      | otherwise        = spriteError


-- Define os tipos de telas
data Screen = SetGridSizeMenu | SetDifficultyMenu | GameScreen deriving Eq

spritePath :: String
spritePath = "assets/sprite.gif"
buttonPath :: String
buttonPath = "assets/button.png"

main :: IO ()
main = do
  withWindow
    1280
    900
    "Campo Minado"
    60
    ( \window -> do
        texture <- managed window $ loadTextureFromImage =<< loadImage =<< getDataFileName spritePath
        buttonTexture <- managed window $ loadTextureFromImage =<< loadImage =<< getDataFileName buttonPath

        let scale = 2 :: Float
        let spriteBombSize = (16* round (scale)) :: Int
        let gridOffset = 100
        initialState <- (gameInit 10 "easy")
        let initialScreen = SetGridSizeMenu

        -- Controle da tela atual com estado encapsulado em uma tupla
        whileWindowOpen_ (\(state, screen) -> do
            case screen of
              SetGridSizeMenu -> do
                drawing $ do
                  clearBackground black
                  -- Configuração dos três botões
                  let buttonSizeWidth = 150
                      buttonSizeHeight = 75
                      buttonSizeSpacing = 20
                      buttonSizeY = 300
                      sizeFontSize = 20
                  -- Configuração dos botões e seus respectivos parâmetros
                  let buttonSizeConfigs = 
                        [ ("10x10", 100, 10)       -- Texto, Posição X, tamanho GRID"
                        , ("15x15", 300, 15)   
                        , ("20x20", 500, 20)       
                        ]
                  -- Função auxiliar para desenhar o botão e detectar cliques
                  let drawAndCheckButtonSizeClick (text, buttonSizeX, gridSize) = do
                        -- Posições para centralizar o texto no botão
                        let textSizeX = buttonSizeX + (buttonSizeWidth `div` 2) - (length text * sizeFontSize `div` 4)
                            textSizeY = buttonSizeY + (buttonSizeHeight `div` 2) - (sizeFontSize `div` 2)
                        
                        -- Desenha o botão na posição especificada
                        drawTexturePro buttonTexture 
                          (Rectangle 0 0 793 205) 
                          (Rectangle (fromIntegral buttonSizeX) (fromIntegral buttonSizeY) 
                                    (fromIntegral buttonSizeWidth) (fromIntegral buttonSizeHeight))
                          (Vector2 0 0) 0 white

                        -- Desenha o texto no botão
                        drawText text textSizeX textSizeY sizeFontSize black

                        -- Verifica se o botão foi clicado
                        mouseSizeX <- getMouseX
                        mouseSizeY <- getMouseY
                        leftMouseSizeClicked <- isMouseButtonPressed MouseButtonLeft
                        let isButtonSizeClicked = leftMouseSizeClicked &&
                              mouseSizeX >= buttonSizeX &&
                              mouseSizeX <= buttonSizeX + buttonSizeWidth &&
                              mouseSizeY >= buttonSizeY &&
                              mouseSizeY <= buttonSizeY + buttonSizeHeight
                        
                        -- Se o botão foi clicado, inicia o jogo com o estado inicial
                        if isButtonSizeClicked
                          then do
                            initialState <- gameInit gridSize ""
                            return (Just (initialState, SetDifficultyMenu)) -- Retorna a próxima tela menu 
                          else return Nothing

                  -- Desenha cada botão e verifica cliques
                  result <- foldM (\acc btnConfig -> 
                            case acc of
                              Just val -> return (Just val) -- Já encontrou o botão clicado
                              Nothing -> drawAndCheckButtonSizeClick btnConfig) 
                            Nothing buttonSizeConfigs

                  -- Decide o próximo estado com base na detecção de clique
                  case result of
                    Just gameState -> return gameState
                    Nothing -> return (state, SetGridSizeMenu) -- Se nenhum botão foi clicado, permanece no menu


              SetDifficultyMenu -> do
                drawing $ do
                  clearBackground black
                  -- Configuração dos três botões
                  let buttonWidth = 150
                      buttonHeight = 75
                      buttonSpacing = 20
                      buttonY = 300
                      fontSize = 20
                  -- Configuração dos botões e seus respectivos parâmetros
                  let buttonConfigs = 
                        [ ("Easy", 100, "easy")       -- Texto, Posição X, Dificuldade"
                        , ("Normal", 300, "normal")   
                        , ("Hard", 500, "hard")       
                        ]
                  -- Função auxiliar para desenhar o botão e detectar cliques
                  let drawAndCheckButtonClick (text, buttonX, difficulty) = do
                        -- Posições para centralizar o texto no botão
                        let textX = buttonX + (buttonWidth `div` 2) - (length text * fontSize `div` 4)
                            textY = buttonY + (buttonHeight `div` 2) - (fontSize `div` 2)

                        -- Desenha o botão na posição especificada
                        drawTexturePro buttonTexture 
                          (Rectangle 0 0 793 205) 
                          (Rectangle (fromIntegral buttonX) (fromIntegral buttonY) 
                                    (fromIntegral buttonWidth) (fromIntegral buttonHeight))
                          (Vector2 0 0) 0 white

                        -- Desenha o texto no botão
                        drawText text textX textY fontSize black

                        -- Verifica se o botão foi clicado
                        mouseX <- getMouseX
                        mouseY <- getMouseY
                        leftMouseClicked <- isMouseButtonPressed MouseButtonLeft
                        let isButtonClicked = leftMouseClicked &&
                              mouseX >= buttonX &&
                              mouseX <= buttonX + buttonWidth &&
                              mouseY >= buttonY &&
                              mouseY <= buttonY + buttonHeight

                        -- Se o botão foi clicado, inicia o jogo com o estado inicial
                        if isButtonClicked
                          then do
                            initialState <- gameInit 10 difficulty
                            return (Just (initialState, GameScreen)) -- Retorna o estado do jogo e tela
                          else return Nothing

                  -- Desenha cada botão e verifica cliques
                  result <- foldM (\acc btnConfig -> 
                            case acc of
                              Just val -> return (Just val) -- Já encontrou o botão clicado
                              Nothing -> drawAndCheckButtonClick btnConfig) 
                            Nothing buttonConfigs

                  -- Decide o próximo estado com base na detecção de clique
                  case result of
                    Just gameState -> return gameState
                    Nothing -> return (state, SetDifficultyMenu) -- Se nenhum botão foi clicado, permanece no menu

              GameScreen -> do
                -- 1. Input
                leftButtonClicked <- isMouseButtonPressed MouseButtonLeft
                rightButtonClicked <- isMouseButtonPressed MouseButtonRight
                let mouseClicked = leftButtonClicked || rightButtonClicked
                (mouseX, mouseY) <- (\(Vector2 x y) -> (floor x, floor y)) <$> getMousePosition

                let col = (mouseX - gridOffset) `div` spriteBombSize
                    row = (mouseY - gridOffset) `div` spriteBombSize
                    validClick = mouseClicked && row >= 0 && col >= 0 &&
                                row < length (state'grid state) &&
                                col < length (head (state'grid state))

                -- 2. Atualizar jogo
                newState <- if validClick
                            then gameUpdate state row col rightButtonClicked
                            else return state
                if validClick then do
                  -- printGrid (state'grid newState)
                  putStrLn $ "-------------------------- "
                  putStrLn $ "Remaining bombs: " ++ show (state'remainingBombs newState)
                  putStrLn $ "       Finished: " ++ show (state'finished newState)
                  putStrLn $ "           Lose: " ++ show (state'lose newState)
                  putStrLn $ "            Cnt: " ++ show (state'cnt newState)
                  -- FIXME: condição para você ganhou está sempre considerando vitória
                  if (state'cnt newState) == (state'win newState) then
                    putStrLn "               Voce ganhou!!"
                    -- TODO: revelar grid final?
                  else if (state'lose newState) then
                    putStrLn "               Voce perdeu!!"
                    -- TODO: revelar grid final
                    -- TODO: impedir que o jogo continue sendo jogado
                    -- TODO: reiniciar jogo?
                  else 
                    putStrLn "                Game running"
                else return ()

                drawing 
                  ( do
                    clearBackground black

                    -- Campo visível
                    forM_ (zip [0..] (state'grid newState)) $ \(rowIndex, rowList) -> 
                        forM_ (zip [0..] rowList) $ \(colIndex, _) -> 
                          ( do
                              let x = fromIntegral (gridOffset + (colIndex * spriteBombSize)) :: Float
                                  y = fromIntegral (gridOffset + (rowIndex * spriteBombSize)) :: Float
                                  rect = getRectForVisibleCellSprite (state'grid newState) rowIndex colIndex
                              
                              if visited ((state'grid newState) !! rowIndex !! colIndex) then
                                drawTexturePro texture spriteVisited
                                  (Rectangle x y ((rectangle'width (spriteVisited))*scale) 
                                  ((rectangle'height (spriteVisited))*scale)) 
                                  (Vector2 0 0) 0 white
                              else
                                drawTexturePro texture spriteNotVisited
                                  (Rectangle x y ((rectangle'width (spriteNotVisited))*scale) 
                                  ((rectangle'height (spriteNotVisited))*scale)) 
                                  (Vector2 0 0) 0 white

                              drawTexturePro texture rect 
                                (Rectangle x y ((rectangle'width (rect))*scale) 
                                ((rectangle'height (rect))*scale)) 
                                (Vector2 0 0) 0 white
                          )

                    -- Campo invisível, debug apenas
                  {- forM_ (zip [0..] (state'grid newState)) $ \(rowIndex, rowList) -> 
                        forM_ (zip [0..] rowList) $ \(colIndex, _) -> 
                          ( do
                              let x = fromIntegral (500 + (colIndex * spriteBombSize)) :: Float
                                  y = fromIntegral (100 + (rowIndex * spriteBombSize)) :: Float
                                  rect = getRectForHiddenCellSprite (state'grid newState) rowIndex colIndex
                              
                              -- if not visited
                              drawTexturePro texture spriteVisited 
                                (Rectangle x y ((rectangle'width (spriteVisited))*scale) 
                                ((rectangle'height (spriteVisited))*scale)) 
                                (Vector2 0 0) 0 white

                              drawTexturePro texture rect 
                                (Rectangle x y ((rectangle'width (rect))*scale) 
                                ((rectangle'height (rect))*scale)) 
                                (Vector2 0 0) 0 white
                          )-}
                  )
                return (newState, GameScreen)
          )
          (initialState, initialScreen) -- Estado inicial e tela inicial
    )


{-

concatMap (map (\x -> x) [[1, 2, 3], [4, 5, 6], [7, 8, 9]])

loop infinito {
    Matheus
    1. Pega input do usuário
        - click do mouse (esq, direito, do meio)
        - movimento do mouse (posição x e y)
        - coord = converter o X, Y pra cood da grid (lista de listas)

    2. Atualização do sistema reagindo ao input do usuário
        - status do jogo = grid_update coord
    
    3. Render/"Pintar na tela"
        Levi
        - Pintar cronomtro
        Yudi
        - Pintar cada Nó da lista
        - Pintar o restante da interface (bordas)
}

-}

