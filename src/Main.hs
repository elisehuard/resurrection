{-# LANGUAGE PackageImports, RecursiveDo #-}
import Resurrection.Types
import Resurrection.Text
import Resurrection.FRP
import Resurrection.Graphics
import Resurrection.Game
import Data.IORef
-- todo is param really necessary?
import FRP.Elerea.Param
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure)
import Graphics.Rendering.OpenGL (GLdouble)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad.Reader
import Debug.Trace


windowCloseCallback closed window = writeIORef closed True

main :: IO ()
main = do 
    let width  = 640
        height = 480
        gameState = Menu

    (keyPress,keyPressSink) <- external Nothing
    (closeGame, closeGameSink) <- external False
    (windowSize,windowSizeSink) <- external (fromIntegral width, (fromIntegral height :: GLdouble))
    closed <- newIORef False

    withWindow width height "Resurrection" $ \win -> do
          initGL width height
          setWindowCloseCallback     win $ Just $ windowCloseCallback closed
          setWindowSizeCallback      win $ Just $ resizeGL windowSizeSink

          -- player grass levelbackground
          
          textures <- loadTextures
          font <- loadFont "fonts/DroidSans.ttf"

          -- All we need to get going is an IO-valued signal and an IO
          -- function to update the external signals
          game <- start $ resurrection windowSize textures font win
          driveNetwork game (readInput win closed)

          -- The inevitable sad ending
          exitWith ExitSuccess


readInput window closed = do
    -- threadDelay 0 -- to avoid continuous polling, normally 20ms by default (see elerea-examples)
    threadDelay 16666

    t <- getTime
    resetTime

    k <- keyIsPressed window Key'Escape

    c <- readIORef closed

    return $ if k || c || (t == Nothing) then Nothing else Just (realToFrac (fromJust t))
