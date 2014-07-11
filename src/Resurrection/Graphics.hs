{-# LANGUAGE PackageImports #-}
module Resurrection.Graphics
(withWindow, initGL)
where

import Graphics.Rendering.OpenGL
import "GLFW-b" Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Control.Monad (when)

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
