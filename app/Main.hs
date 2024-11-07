{-# LANGUAGE PatternSynonyms #-}
module Main where


-- Assets do campo minado
-- https://github.com/BrandonDusseau/minesweeper-classic

import Control.Monad (forM_)
import Paths_CampoMinado (getDataFileName)
import Raylib.Core (clearBackground)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures
  ( 
    drawTexturePro,
    loadImage,
    loadRenderTexture,
    loadTextureFromImage,
  )
import Raylib.Types (Rectangle (Rectangle, rectangle'height, rectangle'width), pattern Vector2)
import Raylib.Util (drawing, textureMode, whileWindowOpen0, whileWindowOpen_, withWindow, managed)
import Raylib.Util.Colors (black, lightGray, white)

import Game (printGrid, gameInit, gameUpdate, state'grid, state'remainingBombs, state'finished, state'cnt, state'lose)
import Node (Node(..))
import Grid (Grid)


getRectForCellSprite :: Grid -> Int -> Int -> Rectangle
getRectForCellSprite grid row col = rect
  where 
    spriteBombError      = (Rectangle (96)   (64) 16 16) -- Área hachurada em rosa

    spriteBombCount0     = (Rectangle (32)   (84) 16 16) -- É apenas um quadrado transparente no .gif
    spriteBombCount1     = (Rectangle (16*0) (21+0) 16 16)
    spriteBombCount2     = (Rectangle (16*1) (21+0) 16 16)
    spriteBombCount3     = (Rectangle (16*2) (21+0) 16 16)
    spriteBombCount4     = (Rectangle (16*3) (21+0) 16 16)
    spriteBombCount5     = (Rectangle (16*0) (21+16) 16 16)
    spriteBombCount6     = (Rectangle (16*1) (21+16) 16 16)
    spriteBombCount7     = (Rectangle (16*2) (21+16) 16 16)
    spriteBombCount8     = (Rectangle (16*3) (21+16) 16 16)

    spriteVisited        = (Rectangle (32) (83) 16 16)
    spriteNotVisited     = (Rectangle (50) (83) 16 16)

    spriteBomb           = (Rectangle (16*2) (21+16*2) 16 16)

    isVisited   = visited (grid !! row !! col)
    bombsAround = dataNode (grid !! row !! col)
    -- spriteFlag           = ...

      -- | i < 0 || j < 0 ==  spriteBombCountError
      -- Usa um mapa ou lista e indexar i*row+j pra pegar o retangulo correto
    rect
      | not isVisited                        = spriteNotVisited
      | not (isVisited) && bombsAround == -1 = spriteBomb
      | not (isVisited) && bombsAround == 0 = spriteVisited
      | not (isVisited) && bombsAround == 1 = spriteBombCount1
      | not (isVisited) && bombsAround == 2 = spriteBombCount2
      | not (isVisited) && bombsAround == 3 = spriteBombCount3
      | not (isVisited) && bombsAround == 4 = spriteBombCount4
      | not (isVisited) && bombsAround == 5 = spriteBombCount5
      | not (isVisited) && bombsAround == 6 = spriteBombCount6
      | not (isVisited) && bombsAround == 7 = spriteBombCount7
      | not (isVisited) && bombsAround == 8 = spriteBombCount8
      | otherwise                          = spriteBombError

spritePath :: String
spritePath = "assets/sprite.gif"

main :: IO ()
main = do
  withWindow
    600
    450
    "Campo Minado"
    60
    ( \window -> do
        texture <- managed window $ loadTextureFromImage =<< loadImage =<< getDataFileName spritePath

        let scale = 2
        -- É o mesmo retângulo que o spriteVisited, como não repetir?
        let bkg = (Rectangle (50) (83) 16 16)

        initialState <- (gameInit 10 "easy")

        whileWindowOpen_
          (\state -> do
            -- 3. Renderizar
            drawing
              ( do
                clearBackground black
                let spriteBombSize = 16*scale

                forM_ (zip [0..] (state'grid state)) $ \(rowIndex, row) -> 
                    forM_ (zip [0..] row) $ \(colIndex, _) -> 
                      ( do
                          let x = 100 + fromIntegral (colIndex * spriteBombSize)
                              y = 100 + fromIntegral (rowIndex * spriteBombSize)
                              rect = getRectForCellSprite (state'grid state) rowIndex colIndex
                          
                          -- if not visited
                          -- drawTexturePro texture bkg 
                          --   (Rectangle x y ((rectangle'width (bkg))*(fromIntegral scale)) 
                          --   ((rectangle'height (bkg))*(fromIntegral scale))) 
                          --   (Vector2 0 0) 0 white

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

