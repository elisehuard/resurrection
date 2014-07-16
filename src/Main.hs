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


data GameState = Menu | Level Int
data Lifeform = Lifeform (Vector2 GLdouble) Species LifeStatus
data LifeStatus = Alive | Dead
data Species = Grass

-- initial player position
playerPos0 = Vector2 200 200
playerWidth = 20 
playerHeight = (50 :: GLdouble)
grassSize = (32 :: GLdouble)

windowCloseCallback closed window = do writeIORef closed True
                                       return ()

main :: IO ()
main = do 
    let width  = 640
        height = 480

    (keyPress,keyPressSink) <- external Nothing
    (closeGame, closeGameSink) <- external False
    (windowSize,windowSizeSink) <- external (fromIntegral width, (fromIntegral height :: GLdouble))
    closed <- newIORef False

    withWindow width height "Resurrection" $ \win -> do
          initGL width height
          setWindowCloseCallback     win $ Just $ windowCloseCallback closed
          setWindowSizeCallback      win $ Just $ resizeGL windowSizeSink

          mbBackground <- loadTexture "images/rocks.jpg"
          -- All we need to get going is an IO-valued signal and an IO
          -- function to update the external signals
          game <- start $ resurrection windowSize mbBackground win
          driveNetwork game (readInput win closed)

          -- The inevitable sad ending
          exitWith ExitSuccess

-- TODO limits of the world - go round?
updateFromKey :: Vector2 GLdouble -> (Bool, Bool, Bool, Bool) -> Vector2 GLdouble
updateFromKey (Vector2 x y) keyP = case keyP of -- todo: all keys pressed considered?
                                     (True, _, _, _)  -> Vector2 (x - 5) y
                                     (_, True, _, _)   -> Vector2 x (y + 5)
                                     (_, _, True, _)  -> Vector2 x (y - 5)
                                     (_, _, _, True) -> Vector2 (x + 5) y
                                     otherwise    -> Vector2 x y

resurrection :: Signal (GLdouble, GLdouble) -> Maybe TextureObject -> Window -> SignalGen Double (Signal (IO())) 
resurrection windowSize mbBackground window  = do 
                                              directionControl <- effectful $ (,,,)
                                                <$> keyIsPressed window Key'Left
                                                <*> keyIsPressed window Key'Up
                                                <*> keyIsPressed window Key'Down
                                                <*> keyIsPressed window Key'Right
                                              fpsTracking <- stateful (0, 0, Nothing) $ \dt (time, count, _) ->
                                                    let time' = time + dt
                                                        done = time > 5
                                                    in if done
                                                    then (0, 0, Just (count / time'))
                                                    else (time', count + 1, Nothing)
                                              playerPos <- transfer playerPos0 (\dt keyP p -> updateFromKey p keyP) directionControl
                                              -- only happen once during game?

                                              return $ (renderLevel mbBackground window) <$> windowSize <*> playerPos <*> fpsTracking


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

-- major refactor needed soon

drawBackground :: (GLdouble, GLdouble) -> Maybe TextureObject -> IO ()
drawBackground (width, height) mbTexture = do
                            texture Texture2D $= Enabled
                            textureFunction $= Replace
                            textureBinding Texture2D $= mbTexture
                            loadIdentity
                            renderPrimitive Quads $ do
                                vertex $ Vertex2 (0 :: GLdouble) 0
                                toTexture (0, 0) 
                                vertex $ Vertex2 width           0
                                toTexture (1, 0)
                                vertex $ Vertex2 width           height
                                toTexture (1, 1)
                                vertex $ Vertex2 0               height
                                toTexture (0, 1)
                            texture Texture2D $= Disabled

toTexture (x,y) = texCoord2f (TexCoord2 x y)
                where texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()

drawPlayer (Vector2 x y) = do
        color $ Color4 0 0 1.0 (1 :: GLfloat)
        loadIdentity
        renderPrimitive Quads $ do
            vertex $ Vertex2 (x - playerWidth/2) (y - playerHeight/2)
            vertex $ Vertex2 (x + playerWidth/2) (y - playerHeight/2)
            vertex $ Vertex2 (x + playerWidth/2) (y + playerHeight/2)
            vertex $ Vertex2 (x - playerWidth/2) (y + playerHeight/2)

drawGrass (Vector2 x y) alive = do
                                    case alive of
                                        Dead -> color $ Color4 0.33 0.41 0.18 (1 :: GLfloat)
                                        Alive -> color $ Color4 0 1.0 0 (1 :: GLfloat)
                                    loadIdentity
                                    renderPrimitive Quads $ do
                                        vertex $ Vertex2 (x - grassSize) (y - grassSize)
                                        vertex $ Vertex2 (x + grassSize) (y - grassSize)
                                        vertex $ Vertex2 (x + grassSize) (y + grassSize)
                                        vertex $ Vertex2 (x - grassSize) (y + grassSize)
                                     

renderLevel :: Maybe TextureObject -> Window -> (GLdouble, GLdouble) -> Vector2 GLdouble -> (Double, Double, Maybe Double) -> IO ()
renderLevel mbBackground window windowSize playerPos (_,_,fps) = do 
                                                case fps of
                                                    Just value -> putStrLn $ "FPS: " ++ show value
                                                    Nothing -> return ()
                                                clear [ColorBuffer, DepthBuffer]
                                                drawBackground windowSize mbBackground
                                                drawGrass (Vector2 300 300) Dead
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
