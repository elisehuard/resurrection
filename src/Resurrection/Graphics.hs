{-# LANGUAGE PackageImports #-}
module Resurrection.Graphics
( initGL
, resizeGL
, withWindow
, keyIsPressed
, resetTime
, loadTexture
, toTexture
, loadTextures
, playerTexture
, backgroundTexture
, renderLevel
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
import Codec.Image.STB (loadImage)
import qualified Data.Map.Strict as Map
import Resurrection.Types
import Debug.Trace

-- constants

playerWidth = 60 
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
                  loaded <- mapM loadTexture ["images/rocks.jpg", "images/alien.png", "images/alien-back.png", "images/alien-right.png", "images/alien-left.png", "images/alien-body.png", "images/alien-leg.png"]
                  let names = ["level-1", "alien-front", "alien-back", "alien-right", "alien-left", "alien-body", "alien-leg"] 
                      list  = zip names loaded
                  return $ Map.fromList list

lookupTexture :: String -> Textures -> Maybe TextureObject
lookupTexture name textures = fromJust (Map.lookup name textures)

toTexture (x,y) = texCoord2f (TexCoord2 x y)
                where texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()

-- draw individual components

--textureName :: (Draw a) => a -> String
playerTexture (Player {direction = Front})   = lookupTexture "alien-front"
playerTexture (Player {direction = GoBack})    = lookupTexture "alien-back"
playerTexture (Player {direction = GoRight})   = lookupTexture "alien-right"
playerTexture (Player {direction = GoLeft})    = lookupTexture "alien-left"
backgroundTexture (Background _ _) = lookupTexture "level-1"

bodyTexture = lookupTexture "alien-body"
legTexture = lookupTexture "alien-leg"

instance Draw Player where
   draw player textures = do 
                            loadIdentity
                            drawLeftLeg player (legTexture textures)
                            drawRightLeg player (legTexture textures)
                            drawBody player (bodyTexture textures)

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

drawRightLeg (Player (Vector2 x y) direction Neutral tick) legTexture =  drawLeg (x + playerWidth/2 - 30) (y - playerHeight/2) 0 0 legTexture

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

instance Draw Lifeform where
  draw (Lifeform (Vector2 x y) Grass alive _) _ = do
                                    case alive of
                                        Dead -> color $ Color4 0.33 0.41 0.18 (1 :: GLfloat)
                                        Alive -> color $ Color4 0.4 1.0 0 (1 :: GLfloat)
                                    loadIdentity
                                    renderPrimitive Quads $ do
                                        vertex $ Vertex2 (x - grassSize) (y - grassSize)
                                        vertex $ Vertex2 (x + grassSize) (y - grassSize)
                                        vertex $ Vertex2 (x + grassSize) (y + grassSize)
                                        vertex $ Vertex2 (x - grassSize) (y + grassSize)

instance Draw Background where
  draw background@(Background _ (width, height)) textures = do  let mbTexture = backgroundTexture background textures
                                                                texture Texture2D $= Enabled
                                                                textureFunction $= Replace
                                                                textureBinding Texture2D $= mbTexture
                                                                loadIdentity
                                                                renderPrimitive Quads $ do
                                                                    toTexture (0, 0) 
                                                                    vertex $ Vertex2 (0 :: GLdouble) 0
                                                                    toTexture (1, 0)
                                                                    vertex $ Vertex2 width           0
                                                                    toTexture (1, 1)
                                                                    vertex $ Vertex2 width           height
                                                                    toTexture (0, 1)
                                                                    vertex $ Vertex2 0               height
                                                                texture Texture2D $= Disabled

-- data World = World Background [Lifeform]
instance Draw World where
  draw (World background lifeforms) textures = do draw background textures
                                                  mapM ((flip draw) textures) lifeforms
                                                  return ()

-- collision detection - is geometry graphics or not?
-- player x - playerWidth/2 left edge
-- grass x - grassSize
-- 
-- todo: define shape type and collisions between several types of shape, instead of between player and lifeform
xcolliding (Lifeform (Vector2 g1 g2) Grass _ _) (Vector2 x y) = ((x - playerWidth/2)  > (g1 - grassSize) && (x - playerWidth/2) < (g1 + grassSize)) ||
                                                              ((x + playerWidth/2) > (g1 - grassSize) && (x + playerWidth/2) < (g1 + grassSize))
ycolliding (Lifeform (Vector2 g1 g2) Grass _ _) (Vector2 x y) = ((y - playerHeight/2)  > (g2 - grassSize) && (x - playerHeight/2) < (g2 + grassSize)) ||
                                                              ((y + playerHeight/2) > (g2 - grassSize) && (x + playerHeight/2) < (g2 + grassSize))

colliding lifeform position = xcolliding lifeform position && ycolliding lifeform position

-- the actual rendering
renderLevel :: Textures -> FTGL.Font -> Window -> (GLdouble, GLdouble) -> World -> Player -> Life -> (Double, Double, Maybe Double) -> IO ()
renderLevel textures font window windowSize world player life (_,_,fps) = do 
                                                case fps of
                                                    Just value -> putStrLn $ "FPS: " ++ show value
                                                    Nothing -> return ()
                                                clear [ColorBuffer, DepthBuffer]
                                                draw world textures
                                                draw player textures
                                                printText font windowSize (Vertex2 (-200) 200) $ show life
                                                flush
                                                swapBuffers window
                                                pollEvents -- Necessary for it not to freeze.
