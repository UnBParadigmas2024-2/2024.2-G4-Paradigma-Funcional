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
import Raylib.Util (drawing, textureMode, whileWindowOpen0, withWindow, managed)
import Raylib.Util.Colors (black, lightGray, white)

data Node = Node { dataNode :: Int, visited :: Bool } deriving Show
type Grid = [[Node]]


-- spriteRedEmpty = (Rectangle (11*0) 0 11 21)
-- spriteRed0     = (Rectangle (11*1) 0 11 21)
-- spriteRed1     = (Rectangle (11*2) 0 11 21)
-- spriteRed2     = (Rectangle (11*3) 0 11 21)
-- spriteRed3     = (Rectangle (11*3) 0 11 21)
-- spriteRed4     = (Rectangle (11*4) 0 11 21)
-- spriteRed5     = (Rectangle (11*2) 0 11 21)
-- spriteRed6     = (Rectangle (11*6) 0 11 21)
-- spriteRed7     = (Rectangle (11*7) 0 11 21)
-- spriteRed8     = (Rectangle (11*8) 0 11 21)
-- spriteRed9     = (Rectangle (11*9) 0 11 21)
-- spriteRedDash  = (Rectangle (11*10) 0 11 21)

getRectForCellSprite :: Grid -> Int -> Int -> Rectangle
getRectForCellSprite table row col = rect
  where 
    spriteBombCountError = (Rectangle (96)   (64) 16 16) -- Área hachurada em rosa
    spriteBombCount0     = (Rectangle (32)   (84) 16 16) -- É apenas um quadrado transparente no .gif
    spriteBombCount1     = (Rectangle (16*0) (21+0) 16 16)
    spriteBombCount2     = (Rectangle (16*1) (21+0) 16 16)
    spriteBombCount3     = (Rectangle (16*2) (21+0) 16 16)
    spriteBombCount4     = (Rectangle (16*3) (21+0) 16 16)
    spriteBombCount5     = (Rectangle (16*0) (21+16) 16 16)
    spriteBombCount6     = (Rectangle (16*1) (21+16) 16 16)
    spriteBombCount7     = (Rectangle (16*2) (21+16) 16 16)
    spriteBombCount8     = (Rectangle (16*3) (21+16) 16 16)

    spriteBomb           = (Rectangle (16*2) (21+16*2) 16 16)

      -- | i < 0 || j < 0 ==  spriteBombCountError
      -- Usa um mapa ou lista e indexar i*row+j pra pegar o retangulo correto
    rect
      | dataNode (table !! row !! col) == -1 = spriteBomb
      | dataNode (table !! row !! col) == 0 = spriteBombCount0
      | dataNode (table !! row !! col) == 1 = spriteBombCount1
      | dataNode (table !! row !! col) == 2 = spriteBombCount2
      | dataNode (table !! row !! col) == 3 = spriteBombCount3
      | dataNode (table !! row !! col) == 4 = spriteBombCount4
      | dataNode (table !! row !! col) == 5 = spriteBombCount5
      | dataNode (table !! row !! col) == 6 = spriteBombCount6
      | dataNode (table !! row !! col) == 7 = spriteBombCount7
      | dataNode (table !! row !! col) == 8 = spriteBombCount8
      | otherwise                           = spriteBombCountError

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
        scale <- return 1
        board <- return ([
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 1 True), (Node 2 True), (Node 3 True), (Node 1 True), (Node 2 True), (Node 3 True)], 
            [(Node 4 True), (Node 5 True), (Node 6 True), (Node 4 True), (Node 5 True), (Node 6 True)],
            [(Node 7 True), (Node 8 True), (Node (-1) True), (Node 7 True), (Node 8 True), (Node (-1) True)]
          ] :: Grid)

        rt <- managed window $ loadRenderTexture 200 200

        bkg <- return (Rectangle (16*2) (83) 16 16)

        whileWindowOpen0
          ( drawing
              ( do
                  textureMode
                    rt
                    ( do
                        clearBackground lightGray
                        drawText "This is scaled up" 10 10 20 black
                    )

                  -- 3. Renderizar
                  clearBackground lightGray
                  let spriteBombSize = 16*scale

                  forM_ (zip [0..] board) $ \(rowIndex, row) -> 
                      forM_ (zip [0..] row) $ \(colIndex, _) -> 
                        ( do
                            let x = 100 + fromIntegral (colIndex * spriteBombSize)
                                y = 100 + fromIntegral (rowIndex * spriteBombSize)
                                rect = getRectForCellSprite board rowIndex colIndex
                            
                            drawTexturePro texture bkg 
                              (Rectangle x y ((rectangle'width (bkg))*(fromIntegral scale)) 
                              ((rectangle'height (bkg))*(fromIntegral scale))) 
                              (Vector2 0 0) 0 white
                            drawTexturePro texture rect 
                              (Rectangle x y ((rectangle'width (rect))*(fromIntegral scale)) 
                              ((rectangle'height (rect))*(fromIntegral scale))) 
                              (Vector2 0 0) 0 white
                        )
              )
          )
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

