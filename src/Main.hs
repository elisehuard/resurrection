{-# LANGUAGE PackageImports #-}
import Control.Applicative hiding (Const)
import Resurrection.FRP
import Resurrection.Graphics
import Data.IORef
import Graphics.Rendering.OpenGL
import FRP.Elerea.Param
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)

data Direction = GoRight | GoLeft | GoUp | GoDown

-- initial player position
playerPos0 = Vector2 0 0
playerWidth = 5 
playerHeight = (5 :: GLdouble)

keyCallback :: ((Maybe Direction) -> IO()) -> (Bool -> IO()) -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback keyPress close win Key'Right _ _ _ = keyPress $ Just GoRight
keyCallback keyPress close win Key'Left  _ _ _ = keyPress $ Just GoLeft
keyCallback keyPress close win Key'Up    _ _ _ = keyPress $ Just GoUp
keyCallback keyPress close win Key'Down  _ _ _ = keyPress $ Just GoDown
keyCallback keyPress close win Key'Escape _ _ _ = close $ True 
keyCallback keyPress _     win _         _ _ _ = return ()

windowCloseCallback closed window = do writeIORef closed True
                                       return ()

main :: IO ()
main = do 
    let width  = 640
        height = 480

    (keyPress,keyPressSink) <- external Nothing
    (closeGame, closeGameSink) <- external False
    (windowSize,windowSizeSink) <- external (0,0)
    closed <- newIORef False

    withWindow width height "Resurrection" $ \win -> do
          initGL width height
          setWindowCloseCallback     win $ Just $ windowCloseCallback closed
          setKeyCallback             win $ Just $ keyCallback keyPressSink closeGameSink
          setWindowSizeCallback      win $ Just $ resizeGL windowSizeSink

          -- All we need to get going is an IO-valued signal and an IO
          -- function to update the external signals
          game <- start $ resurrection keyPress windowSize win
          driveNetwork game (readInput win closed)

          -- The inevitable sad ending
          exitWith ExitSuccess

-- TODO limits of the world - go round?
updateFromKey :: Vector2 GLdouble -> Maybe Direction -> Vector2 GLdouble
updateFromKey (Vector2 x y) keyP = case keyP of
                                     Just GoLeft  -> Vector2 (x - 5) y
                                     Just GoUp    -> Vector2 x (y + 5)
                                     Just GoDown  -> Vector2 x (y - 5)
                                     Just GoRight -> Vector2 (x + 5) y
                                     otherwise    -> Vector2 x y

resurrection :: Signal (Maybe Direction) -> Signal (GLdouble, GLdouble) -> Window -> SignalGen Double (Signal (IO())) 
resurrection keyPress windowSize window  = do 
                                              fpsTracking <- stateful (0, 0, Nothing) $ \dt (time, count, _) ->
                                                    let time' = time + dt
                                                        done = time > 5
                                                    in if done
                                                    then (0, 0, Just (count / time'))
                                                    else (time', count + 1, Nothing)
                                              playerPos <- transfer playerPos0 (\dt keyP p -> updateFromKey p keyP) keyPress
                                              -- playerPos' <- delay playerPos0 playerPos
                                              return $ (renderLevel window) <$> playerPos <*> fpsTracking


readInput window closed = do
    -- threadDelay 0 -- to avoid continuous polling, normally 20ms by default (see elerea-examples)

    t <- getTime
    resetTime

    k <- keyIsPressed window Key'Escape

    c <- readIORef closed

    return $ if k || c || (t == Nothing) then Nothing else Just (realToFrac (fromJust t))

resetTime :: IO ()
resetTime =
    setTime (0 :: Double)

drawPlayer (Vector2 x y) = do
        loadIdentity
        renderPrimitive Quads $ do
            vertex $ Vertex2 (x)             (y)
            vertex $ Vertex2 (x+playerWidth) (y)
            vertex $ Vertex2 (x+playerWidth) (y+playerHeight)
            vertex $ Vertex2 (x)             (y+playerHeight)

resizeGL :: ((GLdouble, GLdouble) -> IO()) -> Window -> Int -> Int -> IO()
resizeGL windowSizeSink window w h = do
                                         windowSizeSink ((fromIntegral w),(fromIntegral h))
                                         viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
                                         matrixMode $= Projection
                                         loadIdentity
                                         ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
                                         matrixMode $= Modelview 0

renderLevel :: Window -> Vector2 GLdouble -> (Double, Double, Maybe Double) -> IO ()
renderLevel window playerPos (_,_,fps) = do 
                                                case fps of
                                                    Just value -> putStrLn $ "FPS: " ++ show value
                                                    Nothing -> return ()
                                                clear [ColorBuffer, DepthBuffer]
                                                color $ Color4 0.2 0.2 0.2 (1 :: GLfloat)
                                                drawPlayer playerPos
                                                flush
                                                swapBuffers window
                                                pollEvents -- Necessary for it not to freeze.

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                       = False

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key
