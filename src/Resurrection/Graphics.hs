{-# LANGUAGE PackageImports #-}
module Resurrection.Graphics
( initGL
, resizeGL
, withWindow
, keyIsPressed
, resetTime
, loadTextures
, renderFrame
, colliding)
where

import Resurrection.Text
import Graphics.Rendering.OpenGL hiding (Front)
import qualified Graphics.Rendering.FTGL as FTGL
import "GLFW-b" Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Control.Monad (when)
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Bitmap.Pure
import Foreign.Ptr (castPtr)
import Foreign.C.Types
import GHC.Float
import Codec.Image.STB (loadImage)
import qualified Data.Map.Strict as Map
import Resurrection.Types
import Debug.Trace

-- constants

playerWidth = 70 
playerHeight = (80 :: GLdouble)
grassSize = (32 :: GLdouble)
legSize = 15

-- general opengl functions

initGL width height = do
  clearColor $= Color4 1 1 1 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  cullFace $= Just Back
  -- size perspective thing
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  matrixMode $= Projection
  loadIdentity
  ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1
  matrixMode $= Modelview 0

resizeGL :: ((GLdouble, GLdouble) -> IO()) -> Window -> Int -> Int -> IO()
resizeGL windowSizeSink window w h = do
                                         windowSizeSink ((fromIntegral w),(fromIntegral h))
                                         viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
                                         matrixMode $= Projection
                                         loadIdentity
                                         ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
                                         matrixMode $= Modelview 0


-- GLFW boilerplate

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                       = False

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

resetTime :: IO ()
resetTime =
    setTime (0 :: Double)

-- textures

compileTexture2D :: Bool -> Bool -> Bitmap Word8 -> IO (Maybe TextureObject)
compileTexture2D isMip isClamped bitmap = do
    mbTexName <- fmap Just genObjectName
    textureBinding Texture2D $= mbTexName
    let (width,height) = bitmapSize bitmap
        wrapMode = case isClamped of
            True    -> ClampToEdge
            False   -> Repeat
        (minFilter,maxLevel) = case isMip of
            False   -> ((Linear', Nothing),0)
            True    -> ((Linear', Just Linear'), floor $ log (fromIntegral $ max width height) / log 2)
    textureWrapMode Texture2D S $= (Repeated, wrapMode) -- or ClampToEdge if bigger than shape. if clam s and t from 0 to 1, if repeat then depends on repeats
    textureWrapMode Texture2D T $= (Repeated, wrapMode)
    textureFilter Texture2D $= (minFilter, Linear')
    textureLevelRange Texture2D $= (0, fromIntegral maxLevel)
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = RGBA8
            dataFormat      = case nchn of
                3   -> RGB
                4   -> RGBA
                _   -> error "unsupported texture format!"
        texImage2D Texture2D NoProxy 0 internalFormat (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 $ PixelData dataFormat UnsignedByte $ castPtr ptr
    when isMip $ generateMipmap' Texture2D
    return mbTexName

loadTexture :: String -> IO (Maybe TextureObject)
loadTexture path = do
    Right img <- loadImage path
    mbTexName <- compileTexture2D False True img
    return mbTexName

loadTextures :: IO Textures
loadTextures = do 
                  let textures = [("level-1", "images/background-1.png")
                                , ("between-1", "images/between-1.png")
                                , ("alien-front", "images/alien.png")
                                , ("alien-back", "images/alien-back.png")
                                , ("alien-right", "images/alien-right.png")
                                , ("alien-left", "images/alien-left.png")
                                , ("alien-body", "images/alien-body.png")
                                , ("alien-body-right", "images/alien-body-right.png")
                                , ("alien-body-left", "images/alien-body-left.png")
                                , ("alien-body-back", "images/alien-body-back.png")
                                , ("alien-leg", "images/alien-leg.png")
                                , ("alien-leg-right", "images/alien-leg-side.png")
                                , ("alien-leg-left", "images/alien-leg-left.png")
                                , ("grass-dead", "images/grass-dead.png")
                                , ("grass-alive", "images/grass-alive.png")
                                , ("rabbit-dead", "images/rabbit-dead.png")
                                , ("rabbit-alive", "images/rabbit-alive.png")]
                  list <- mapM (\(name, file) -> do 
                                  texture <- loadTexture file
                                  return $ (,) name texture) textures

                  return $ Map.fromList list

lookupTexture :: String -> Textures -> Maybe TextureObject
lookupTexture name textures = fromJust (Map.lookup name textures)

toTexture (x,y) = texCoord2f (TexCoord2 x y)
                where texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()

--textureName :: (Draw a) => a -> String
backgroundTexture (Background (Level _) _) = lookupTexture "level-1"
backgroundTexture (Background (Between _) _) = lookupTexture "between-1"

bodyTexture (Player {direction = Front}) = lookupTexture "alien-body"
bodyTexture (Player {direction = GoBack}) = lookupTexture "alien-body-back"
bodyTexture (Player {direction = GoLeft}) = lookupTexture "alien-body-left"
bodyTexture (Player {direction = GoRight}) = lookupTexture "alien-body-right"
legTexture (Player {direction = Front}) = lookupTexture "alien-leg"
legTexture (Player {direction = GoBack}) = lookupTexture "alien-leg"
legTexture (Player {direction = GoLeft}) = lookupTexture "alien-leg-left"
legTexture (Player {direction = GoRight}) = lookupTexture "alien-leg-right"

drawPlayer player textures = do 
                                loadIdentity
                                drawLeftLeg player (legTexture player textures)
                                drawRightLeg player (legTexture player textures)
                                drawBody player (bodyTexture player textures)

drawBody (Player (Vector2 x y) direction _ _) bodyTexture = do 
                                                            texture Texture2D $= Enabled
                                                            textureFunction $= Replace
                                                            textureBinding Texture2D $= bodyTexture
                                                            loadIdentity
                                                            renderPrimitive Quads $ do
                                                                toTexture (0,1)
                                                                vertex $ Vertex2 (x - playerWidth/2) (y - playerHeight/2)
                                                                toTexture (1,1)
                                                                vertex $ Vertex2 (x + playerWidth/2) (y - playerHeight/2)
                                                                toTexture (1,0)
                                                                vertex $ Vertex2 (x + playerWidth/2) (y + playerHeight/2)
                                                                toTexture (0,0)
                                                                vertex $ Vertex2 (x - playerWidth/2) (y + playerHeight/2)
                                                            texture Texture2D $= Disabled


drawLeftLeg (Player (Vector2 x y) direction Neutral tick) legTexture = drawLeg (x - playerWidth/2 + 10) (y - playerHeight/2) 0 0 legTexture
drawLeftLeg (Player (Vector2 x y) Front Walking tick) legTexture = do 
                                                        let animy = realToFrac $ sin tick
                                                        drawLeg (x - playerWidth/2 + 10) (y - playerHeight/2) 0 animy legTexture
drawLeftLeg (Player (Vector2 x y) GoRight Walking tick) legTexture = do 
                                                        let animx = realToFrac $ sin tick
                                                        drawLeg (x - 5) (y - playerHeight/2) animx 0 legTexture
drawLeftLeg (Player (Vector2 x y) GoLeft Walking tick) legTexture = do 
                                                        let animx = realToFrac $ sin tick
                                                        drawLeg (x - 15) (y - playerHeight/2) animx 0 legTexture
drawLeftLeg (Player (Vector2 x y) GoBack Walking tick) legTexture = do 
                                                        let animy = realToFrac $ sin tick
                                                        drawLeg (x - playerWidth/2 + 10) (y - playerHeight/2) 0 animy legTexture

drawRightLeg (Player (Vector2 x y) direction Neutral _) legTexture =  drawLeg (x + playerWidth/2 - 30) (y - playerHeight/2) 0 0 legTexture

drawRightLeg (Player (Vector2 x y) Front Walking tick) legTexture = do 
                                                        let animy = realToFrac $ cos tick
                                                        drawLeg x (y - playerHeight/2) 0 animy legTexture
drawRightLeg (Player (Vector2 x y) GoRight Walking tick) legTexture = do 
                                                        let animx = realToFrac $ cos tick
                                                        drawLeg (x - 5) (y - playerHeight/2) animx 0 legTexture
drawRightLeg (Player (Vector2 x y) GoLeft Walking tick) legTexture = do 
                                                        let animx = realToFrac $ cos tick
                                                        drawLeg (x - 15) (y - playerHeight/2) animx 0 legTexture
drawRightLeg (Player (Vector2 x y) GoBack Walking tick) legTexture = do 
                                                        let animy = realToFrac $ cos tick
                                                        drawLeg x (y - playerHeight/2) 0 animy legTexture
drawLeg x y animx animy legTexture = do 
                                    let amplitudeX = 10
                                        amplitudeY = 5
                                    texture Texture2D $= Enabled
                                    textureFunction $= Replace
                                    textureBinding Texture2D $= legTexture
                                    loadIdentity
                                    renderPrimitive Quads $ do
                                                toTexture (0,1)
                                                vertex $ Vertex2 (x + animx*amplitudeX) (y - legSize/2 + animy*amplitudeY)
                                                toTexture (1,1)
                                                vertex $ Vertex2 (x + 20 + animx*amplitudeX) (y - legSize/2 + animy*amplitudeY)
                                                toTexture (1,0)
                                                vertex $ Vertex2 (x + 20 + animx*amplitudeX) (y + legSize/2 + animy*amplitudeY)
                                                toTexture (0,0)
                                                vertex $ Vertex2 (x + animx*amplitudeX) (y + legSize/2 + animy*amplitudeY)
                                    preservingMatrix $ do
                                        translate (Vector3 0 (-20) (0 :: GLdouble))
                                    texture Texture2D $= Disabled

lifeformTexture (Lifeform _ Grass Alive _) = lookupTexture "grass-alive"
lifeformTexture (Lifeform _ Grass Dead _) = lookupTexture "grass-dead"
lifeformTexture (Lifeform _ Rabbit Alive _) = lookupTexture "rabbit-alive" -- but that will be hopping soon :)
lifeformTexture (Lifeform _ Rabbit Dead _) = lookupTexture "rabbit-dead"

-- fade going from 0 to 1 ?
fadeInPlayer textures fade = do let x = 300
                                    y = 240
                                drawPlayer Player {pos = Vector2 x y, direction = Front, action = Neutral, tick = 0 } textures
                                color $ Color4 0 0 0 (1 - fade)
                                renderPrimitive Quads $ do
                                    vertex $ Vertex2 (x - playerWidth/2) (y - playerHeight/2 - 10)
                                    vertex $ Vertex2 (x + playerWidth/2) (y - playerHeight/2 - 10)
                                    vertex $ Vertex2 (x + playerWidth/2) (y + playerHeight/2)
                                    vertex $ Vertex2 (x - playerWidth/2) (y + playerHeight/2)
                                color $ Color4 1 1 1 (1 :: GLfloat)

drawLifeform lifeform@(Lifeform (Vector2 x y) _ alive _) textures = do
                                    let mbTexture = lifeformTexture lifeform textures
                                    texture Texture2D $= Enabled
                                    textureFunction $= Replace
                                    textureBinding Texture2D $= mbTexture
                                    loadIdentity
                                    renderPrimitive Quads $ do
                                        toTexture (0,1)
                                        vertex $ Vertex2 (x - grassSize) (y - grassSize)
                                        toTexture (1,1)
                                        vertex $ Vertex2 (x + grassSize) (y - grassSize)
                                        toTexture (1,0)
                                        vertex $ Vertex2 (x + grassSize) (y + grassSize)
                                        toTexture (0,0)
                                        vertex $ Vertex2 (x - grassSize) (y + grassSize)
                                    texture Texture2D $= Disabled

drawBackground background@(Background _ (width, height)) textures = do
                                    let mbTexture = backgroundTexture background textures
                                    --activeTexture $= TextureUnit 0
                                    texture Texture2D $= Enabled
                                    textureBinding Texture2D $= mbTexture
                                    textureFunction $= Modulate
                                    loadIdentity
                                    renderPrimitive Quads $ do
                                        toTexture (0, 1) 
                                        vertex $ Vertex2 (0 :: GLdouble) 0
                                        toTexture (1, 1)
                                        vertex $ Vertex2 width           0
                                        toTexture (1, 0)
                                        vertex $ Vertex2 width           height
                                        toTexture (0, 0)
                                        vertex $ Vertex2 0               height
                                    -- texture Texture2D $= Disabled

-- data World = World Background [Lifeform]
drawWorld (World background lifeforms) textures = do 
                                                  drawBackground background textures
                                                  mapM ((flip drawLifeform) textures) lifeforms
                                                  return ()

-- collision detection - is geometry graphics or not?
-- player x - playerWidth/2 left edge
-- grass x - grassSize
-- 
-- todo: define shape type and collisions between several types of shape, instead of between player and lifeform
xcolliding (Lifeform (Vector2 g1 g2) _ _ _) (Vector2 x y) = ((x - playerWidth/2)  > (g1 - grassSize) && (x - playerWidth/2) < (g1 + grassSize)) ||
                                                              ((x + playerWidth/2) > (g1 - grassSize) && (x + playerWidth/2) < (g1 + grassSize))
ycolliding (Lifeform (Vector2 g1 g2) _ _ _) (Vector2 x y) = ((y - playerHeight/2)  > (g2 - grassSize) && (x - playerHeight/2) < (g2 + grassSize)) ||
                                                              ((y + playerHeight/2) > (g2 - grassSize) && (x + playerHeight/2) < (g2 + grassSize))

colliding lifeform position = xcolliding lifeform position && ycolliding lifeform position

-- the actual rendering
renderFrame :: Textures -> FTGL.Font -> Window -> (GLdouble, GLdouble) -> LevelState -> IO ()
renderFrame textures font window windowSize (LevelState level state world player life success) = do 
                                                clear [ColorBuffer, DepthBuffer]
                                                drawWorld world textures
                                                drawPlayer player textures
                                                let fontColor = Color4 0 0 0 (1 :: GLfloat)
                                                color $ fontColor
                                                printText font 24 windowSize (Vertex2 (-260) 200) $ show life
                                                case state of
                                                  Introduction -> do
                                                     coverQuad windowSize 1
                                                     color $ Color4 1 1 1 (1 :: GLfloat)
                                                     introductionText level windowSize font
                                                  FadeIn n -> coverQuad windowSize n
                                                  FadeOut n -> do
                                                     coverQuad windowSize n
                                                     color $ Color4 1 1 1 (1 :: GLfloat)
                                                     printText font 24 windowSize (Vertex2 (-100) 0) "Congratulations!"
                                                  End -> do
                                                     coverQuad windowSize 1
                                                     color $ Color4 1 1 1 (1 :: GLfloat)
                                                     printText font 24 windowSize (Vertex2 (-100) 0) "Congratulations!"
                                                  _ -> return () -- playing hence nothing needs to happen
                                                color $ Color4 1 1 1 (1 :: GLfloat)
                                                flush
                                                swapBuffers window
                                                pollEvents -- Necessary for it not to freeze.

renderFrame textures font window windowSize (InBetweenState level world fade) = do 
                                                clear [ColorBuffer, DepthBuffer]
                                                drawWorld world textures
                                                let fadeGLDouble = (realToFrac fade :: CDouble)
                                                    toGLFloat (CDouble c) = CFloat $ double2Float c
                                                fadeInPlayer textures $ toGLFloat fadeGLDouble
                                                flush
                                                swapBuffers window
                                                pollEvents -- Necessary for it not to freeze.

coverQuad (width, height) fade = do
   color $ Color4 0 0 0 (fade :: GLfloat)
   let x = (320 :: GLdouble)
       y = (240 :: GLdouble)
   loadIdentity
   renderPrimitive Quads $ do
           vertex $ Vertex2 0 (0 :: GLdouble)
           vertex $ Vertex2 width 0
           vertex $ Vertex2 width height
           vertex $ Vertex2 0 height

introductionText (Level 1) windowSize font = do
                                let line1 = "This world needs some grass to get it going."
                                    line2 = "Go to the dead grass and push space bar to"
                                    line3 = "donate some of your life to it!"
                                    end = "press <enter> to get started"
                                printText font 18 windowSize (Vertex2 (-260) 100) line1
                                printText font 18 windowSize (Vertex2 (-260) 50) line2
                                printText font 18 windowSize (Vertex2 (-260) 0) line3
                                printText font 18 windowSize (Vertex2 (-260) (-80)) end
introductionText (Level 2) windowSize font = do
                                let line1 = "we need to add some rabbits to the mix,"
                                    line2 = "but they are expensive!"
                                    line3 = "Kill some grass with key k."
                                    end = "press enter to get started"
                                printText font 18 windowSize (Vertex2 (-260) 100) line1
                                printText font 18 windowSize (Vertex2 (-260) 50) line2
                                printText font 18 windowSize (Vertex2 (-260) 0) line3
                                printText font 18 windowSize (Vertex2 (-260) (-80)) end

