{-# LANGUAGE PackageImports #-}
import Control.Applicative hiding (Const)
import Resurrection.Types
import Resurrection.FRP
import Resurrection.Graphics
import Data.IORef
import Graphics.Rendering.OpenGL
import FRP.Elerea.Param
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad.Reader
import Debug.Trace

-- initial player position
playerPos0 = Vector2 200 200
initialPlayer = Player playerPos0 Neutral
initialWorld = World (Background (Level 1) (640, 480)) [Lifeform (Vector2 300 300) Grass Dead]

windowCloseCallback closed window = do writeIORef closed True
                                       return ()

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

          -- All we need to get going is an IO-valued signal and an IO
          -- function to update the external signals
          game <- start $ resurrection windowSize textures win
          driveNetwork game (readInput win closed)

          -- The inevitable sad ending
          exitWith ExitSuccess

-- TODO limits of the world - go round?
updateFromKey :: Player -> (Bool, Bool, Bool, Bool) -> Player
updateFromKey (Player (Vector2 x y) _) keyP = case keyP of -- todo: all keys pressed considered?
                                     (True, _, _, _)  -> Player (Vector2 (x - 5) y) GoLeft
                                     (_, True, _, _)   -> Player (Vector2 x (y + 5)) GoBack
                                     (_, _, True, _)  -> Player (Vector2 x (y - 5)) Neutral
                                     (_, _, _, True) -> Player (Vector2 (x + 5) y) GoRight
                                     otherwise    -> Player (Vector2 x y) Neutral

resurrection :: Signal (GLdouble, GLdouble) -> Textures -> Window -> SignalGen Double (Signal (IO())) 
resurrection windowSize textures window  = do 
                                              directionControl <- effectful $ (,,,)
                                                <$> keyIsPressed window Key'Left
                                                <*> keyIsPressed window Key'Up
                                                <*> keyIsPressed window Key'Down
                                                <*> keyIsPressed window Key'Right
                                              resurrectControl <- effectful $ keyIsPressed window Key'Space
                                              fpsTracking <- stateful (0, 0, Nothing) $ \dt (time, count, _) ->
                                                    let time' = time + dt
                                                        done = time > 5
                                                    in if done
                                                    then (0, 0, Just (count / time'))
                                                    else (time', count + 1, Nothing)

                                              player <- transfer initialPlayer (\dt keyP p -> updateFromKey p keyP) directionControl
                                              -- state of the world
                                              world <- transfer2 initialWorld (\dt p r w -> worldEvolution p r w) player resurrectControl

                                              return $ (renderLevel textures window) <$> windowSize <*> world <*> player <*> fpsTracking


-- if player stands on lifeform, and lifeform is dead, and they press space (for resurrect), then the lifeform resurrects
-- todo next step: points accounting (lifeform cost + player life down)
-- todo: evolution of the life going from player to thing + way to represent this (text first step?)
worldEvolution (Player position _) True (World background lifeforms) = World background (resurrect lifeforms position)
worldEvolution _                   False world                       = world

resurrect lifeforms position = map (\lifeform -> resurrectColliding (colliding lifeform position) lifeform) lifeforms

-- for now, but accounting will change this.
resurrectColliding True (Lifeform pos species _) = Lifeform pos species Alive
resurrectColliding False lifeform = lifeform

readInput window closed = do
    -- threadDelay 0 -- to avoid continuous polling, normally 20ms by default (see elerea-examples)

    t <- getTime
    resetTime

    k <- keyIsPressed window Key'Escape

    c <- readIORef closed

    return $ if k || c || (t == Nothing) then Nothing else Just (realToFrac (fromJust t))

renderLevel :: Textures -> Window -> (GLdouble, GLdouble) -> World -> Player -> (Double, Double, Maybe Double) -> IO ()
renderLevel textures window windowSize world player (_,_,fps) = do 
                                                case fps of
                                                    Just value -> putStrLn $ "FPS: " ++ show value
                                                    Nothing -> return ()
                                                clear [ColorBuffer, DepthBuffer]
                                                draw world textures
                                                draw player textures
                                                flush
                                                swapBuffers window
                                                pollEvents -- Necessary for it not to freeze.
