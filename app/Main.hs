{-# LANGUAGE PatternSynonyms #-}
module Main where


-- Assets do campo minado
-- https://github.com/BrandonDusseau/minesweeper-classic

import Paths_CampoMinado (getDataFileName)
import Raylib.Core (clearBackground)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures
  ( drawTexture,
    drawTexturePro,
    genImagePerlinNoise,
    loadImage,
    loadRenderTexture,
    loadTextureFromImage,
  )
import Raylib.Types (Rectangle (Rectangle), RenderTexture (renderTexture'texture), pattern Vector2)
import Raylib.Util (drawing, textureMode, whileWindowOpen0, withWindow, managed)
import Raylib.Util.Colors (black, lightGray, orange, white)

logoPath :: String
logoPath = "assets/sprite.gif"

main :: IO ()
main = do
  withWindow
    600
    450
    "Campo Minado"
    60
    ( \window -> do
        texture <- managed window $ loadTextureFromImage =<< genImagePerlinNoise 600 450 20 20 2
        logo <- managed window $ loadTextureFromImage =<< loadImage =<< getDataFileName logoPath
        rt <- managed window $ loadRenderTexture 200 200

        whileWindowOpen0
          ( drawing
              ( do
                  textureMode
                    rt
                    ( do
                        clearBackground lightGray
                        drawText "This is scaled up" 10 10 20 black
                    )

                  clearBackground white
                  drawTexture texture 0 0 orange
                  -- drawTexturePro (renderTexture'texture rt) (Rectangle 0 0 200 (-200)) (Rectangle 50 50 300 300) (Vector2 0 0) 0 white
                  drawTexturePro logo (Rectangle 0 0 132 101) (Rectangle 0 0 (132*2) (101*2)) (Vector2 0 0) 0 white
                  -- drawTexturePro logo (Rectangle (11*4) 0 (11) (21)) (Rectangle 0 0 (11*2) (21*2)) (Vector2 0 0) 0 white
              )
          )
    )