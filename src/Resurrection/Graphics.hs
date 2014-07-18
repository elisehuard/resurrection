{-# LANGUAGE PackageImports #-}
module Resurrection.Graphics
(withWindow, initGL, resizeGL, loadTexture, toTexture, loadTextures, playerTexture, backgroundTexture)
where

import Graphics.Rendering.OpenGL
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

-- constants

playerWidth = 60 
playerHeight = (80 :: GLdouble)
grassSize = (32 :: GLdouble)

-- general opengl functions

initGL width height = do
  clearColor $= Color4 0 0 0 1
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
                  loaded <- mapM loadTexture ["images/rocks.jpg", "images/alien.png", "images/alien-back.png", "images/alien-right.png", "images/alien-left.png"]
                  let names = ["level-1", "alien-neutral", "alien-back", "alien-right", "alien-left"] 
                      list  = zip names loaded
                  return $ Map.fromList list

lookupTexture :: String -> Textures -> Maybe TextureObject
lookupTexture name textures = fromJust (Map.lookup name textures)

toTexture (x,y) = texCoord2f (TexCoord2 x y)
                where texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()

-- draw individual components

--textureName :: (Draw a) => a -> String
playerTexture (Player _ Neutral)   = lookupTexture "alien-neutral"
playerTexture (Player _ GoBack)    = lookupTexture "alien-back"
playerTexture (Player _ GoRight)   = lookupTexture "alien-right"
playerTexture (Player _ GoLeft)    = lookupTexture "alien-left"
backgroundTexture (Background _ _) = lookupTexture "level-1"

instance Draw Player where
  draw (Player (Vector2 x y) direction) mbTexture = do
                                                texture Texture2D $= Enabled
                                                textureFunction $= Replace
                                                textureBinding Texture2D $= mbTexture
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

instance Draw Lifeform where
  draw (Lifeform (Vector2 x y) Grass alive) Nothing = do
                                    case alive of
                                        Dead -> color $ Color4 0.33 0.41 0.18 (1 :: GLfloat)
                                        Alive -> color $ Color4 0 1.0 0 (1 :: GLfloat)
                                    loadIdentity
                                    renderPrimitive Quads $ do
                                        vertex $ Vertex2 (x - grassSize) (y - grassSize)
                                        vertex $ Vertex2 (x + grassSize) (y - grassSize)
                                        vertex $ Vertex2 (x + grassSize) (y + grassSize)
                                        vertex $ Vertex2 (x - grassSize) (y + grassSize)
  draw _ _ = error "what here?"

instance Draw Background where
  draw (Background (Level 1) (width, height)) mbTexture = do
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
