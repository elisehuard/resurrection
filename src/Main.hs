{-# LANGUAGE PackageImports #-}
import Control.Applicative hiding (Const)
import Resurrection.FRP
import Resurrection.Graphics
import Data.IORef
import Graphics.Rendering.OpenGL
import FRP.Elerea.Param
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT hiding (position, scale)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)

data Direction = GoRight | GoLeft | GoUp | GoDown

-- initial player position
playerPos0 = Vector2 0 0
playerWidth = 50
playerHeight = (50 :: GLdouble)

funnelKeys keyPress          (SpecialKey KeyRight) Down _ _ = do keyPress $ Just GoRight
                                                                 return ()
funnelKeys keyPress          (SpecialKey KeyLeft)  Down _ _ = do keyPress $ Just GoLeft
                                                                 return ()
funnelKeys keyPress          (SpecialKey KeyUp)    Down _ _ = do keyPress $ Just GoUp
                                                                 return ()
funnelKeys keyPress          (SpecialKey KeyDown)  Down _ _ = do keyPress $ Just GoDown
                                                                 return ()
--funnelKeys keyPress closeKey (Char '\27')          Down _ _ = closeKey True
funnelKeys keyPress          _                     _    _ _ = do keyPress $ Nothing
                                                                 return ()

writeClose :: IORef Bool -> IO ()
writeClose closed = writeIORef closed True

main :: IO ()
main = do 
          initCommon

          closed <- newIORef False
          closeCallback $= Just (writeClose closed)

          -- input signals (start and sink)
          -- (mousePosition,mousePositionSink) <- external (0,0)
          (keyPress,keyPressSink) <- external Nothing
          --(closeKey, closeKeySink) <- external False
          keyboardMouseCallback $= Just (funnelKeys keyPressSink)

          -- Wrapping up the init phase
          -- closed <- newIORef False

          -- All we need to get going is an IO-valued signal and an IO
          -- function to update the external signals
          game <- start $ resurrection keyPress
          driveNetwork game (readInput closed)

          -- The inevitable sad ending
          exitWith ExitSuccess

-- TODO limits of the world - go round?
updateFromKey (Vector2 x y) keyP = case keyP of
                                    Just GoLeft  -> Vector2 (x - 5) y
                                    Just GoUp    -> Vector2 x (y + 5)
                                    Just GoDown  -> Vector2 x (y - 5)
                                    Just GoRight -> Vector2 (x + 5) y
                                    otherwise    -> Vector2 x y

resurrection keyPress = do 
                           playerPos <- transfer playerPos0 (\dt keyP p -> updateFromKey p keyP) keyPress
                           -- playerPos' <- delay playerPos0 playerPos

                           return $ renderLevel <$> playerPos


readInput closed = do
    threadDelay 0 -- to avoid continuous polling, normally 20ms by default (see elerea-examples)

    t <- getTime
    resetTime

    c <- readIORef closed

    --updateFPS s t

    return $ if c || (t == Nothing) then Nothing else Just (realToFrac (fromJust t))

resetTime :: IO ()
resetTime =
    setTime (0 :: Double)

drawPlayer x y = do
        loadIdentity
        renderPrimitive Quads $ do
            vertex $ Vertex2 (x)             (y)
            vertex $ Vertex2 (x+playerWidth) (y)
            vertex $ Vertex2 (x+playerWidth) (y+playerHeight)
            vertex $ Vertex2 (x)             (y+playerHeight)

renderLevel (Vector2 x y) = do clear [ ColorBuffer ]
                               color $ Color4 0.2 0.2 0.2 (1 :: GLfloat)
                               drawPlayer x y
                               flush
                               Graphics.UI.GLUT.swapBuffers
