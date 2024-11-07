{-# LANGUAGE PatternSynonyms #-}
module Main where


-- Assets do campo minado
-- https://github.com/BrandonDusseau/minesweeper-classic

import Control.Monad (forM_)
import Paths_CampoMinado (getDataFileName)
import Raylib.Core (clearBackground)
import Raylib.Core.Textures
  ( 
    drawTexturePro,
    loadImage,
    loadTextureFromImage,
  )
import Raylib.Types (Rectangle (Rectangle, rectangle'height, rectangle'width), pattern Vector2)
import Raylib.Util (drawing, whileWindowOpen_, withWindow, managed)
import Raylib.Util.Colors (black, white)

import Game (printGrid, gameInit, gameUpdate, state'grid, state'remainingBombs, state'finished, state'cnt, state'lose)
import Node (Node(..), bomba)
import Grid (Grid)

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
-- spriteFlag           = ...

getRectForVisibleCellSprite :: Grid -> Int -> Int -> Rectangle
getRectForVisibleCellSprite grid row col = rect
  where 
    isVisited   = visited (grid !! row !! col)
    bombsAround = dataNode (grid !! row !! col)
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


spritePath :: String
spritePath = "assets/sprite.gif"

main :: IO ()
main = do
  withWindow
    (600 * 2)
    450
    "Campo Minado"
    60
    ( \window -> do
        texture <- managed window $ loadTextureFromImage =<< loadImage =<< getDataFileName spritePath

        let scale = 2

        initialState <- (gameInit 10 "easy")

        whileWindowOpen_
          (\state -> do
            -- 3. Renderizar
            drawing
              ( do
                clearBackground black
                let spriteBombSize = 16*scale

                -- Campo visível
                forM_ (zip [0..] (state'grid state)) $ \(rowIndex, row) -> 
                    forM_ (zip [0..] row) $ \(colIndex, _) -> 
                      ( do
                          let x = 100 + fromIntegral (colIndex * spriteBombSize)
                              y = 100 + fromIntegral (rowIndex * spriteBombSize)
                              rect = getRectForVisibleCellSprite (state'grid state) rowIndex colIndex
                          
                          if visited ((state'grid state) !! rowIndex !! colIndex) then
                            drawTexturePro texture spriteVisited
                              (Rectangle x y ((rectangle'width (spriteVisited))*(fromIntegral scale)) 
                              ((rectangle'height (spriteVisited))*(fromIntegral scale))) 
                              (Vector2 0 0) 0 white
                          else
                            drawTexturePro texture spriteNotVisited
                              (Rectangle x y ((rectangle'width (spriteNotVisited))*(fromIntegral scale)) 
                              ((rectangle'height (spriteNotVisited))*(fromIntegral scale))) 
                              (Vector2 0 0) 0 white

                          drawTexturePro texture rect 
                            (Rectangle x y ((rectangle'width (rect))*(fromIntegral scale)) 
                            ((rectangle'height (rect))*(fromIntegral scale))) 
                            (Vector2 0 0) 0 white
                      )

                -- Campo invisível, debug apenas
                forM_ (zip [0..] (state'grid state)) $ \(rowIndex, row) -> 
                    forM_ (zip [0..] row) $ \(colIndex, _) -> 
                      ( do
                          let x = 500 + fromIntegral (colIndex * spriteBombSize)
                              y = 100 + fromIntegral (rowIndex * spriteBombSize)
                              rect = getRectForHiddenCellSprite (state'grid state) rowIndex colIndex
                          
                          -- if not visited
                          drawTexturePro texture spriteVisited 
                            (Rectangle x y ((rectangle'width (spriteVisited))*(fromIntegral scale)) 
                            ((rectangle'height (spriteVisited))*(fromIntegral scale))) 
                            (Vector2 0 0) 0 white

                          drawTexturePro texture rect 
                            (Rectangle x y ((rectangle'width (rect))*(fromIntegral scale)) 
                            ((rectangle'height (rect))*(fromIntegral scale))) 
                            (Vector2 0 0) 0 white
                      )
              )

            -- 1. Input
            putStrLn $ "Remaining bombs: " ++ show (state'remainingBombs state)
            putStrLn $ "       Finished: " ++ show (state'finished state)
            putStrLn $ "            Cnt: " ++ show (state'cnt state)
            putStrLn $ "           Lose: " ++ show (state'lose state)
            printGrid (state'grid state)
            putStrLn "Enter your move \"row col\" (1/n 1/n):"
            move <- getLine
            let [row, col] = map (\x -> read x - 1) (words move) :: [Int]

            -- 2. Update
            gameUpdate state row col
          )
          initialState
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

