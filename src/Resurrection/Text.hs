module Resurrection.Text where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL

loadFont :: String -> IO Font
loadFont path = createTextureFont path

printText :: Color4 GLfloat -> Font ->  (GLdouble, GLdouble) -> Vertex2 GLdouble -> String -> IO ()
printText color4 font (width, height) (Vertex2 x y) text = do
   do color $ color4
      let xpos = (realToFrac width/2) + x 
          ypos = (realToFrac height/2) + y
      setFontFaceSize font 24 72
      loadIdentity
      translate (Vector3 xpos ypos (0 :: GLdouble))
      renderFont font text Graphics.Rendering.FTGL.Front
