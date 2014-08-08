module Resurrection.Sound where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Sound.ALUT hiding (Static)
import System.IO ( hPutStrLn, stderr )
import Data.List (intersperse)
import Control.Monad (unless)
import Resurrection.Types

backgroundMusic :: String -> IO ()
backgroundMusic path = do
    withProgNameAndArgs runALUT $ \progName args -> do
        -- Create an AL buffer from the given sound file.
        buf <- createBuffer (File path)
        source <- genObjectName
        buffer source $= Just buf
        loopSound source

loopSound source = do
        state <- get (sourceState source)
        if (state == Playing) then
          threadDelay 100
        else
          play [source]
        loopSound source

playSound path = do
  -- ALUT already initialized with background music but this feels brittle :/
  --withProgNameAndArgs runALUT $ \progName args -> do
        buf <- createBuffer (File path)
        -- Generate a single source, attach the buffer to it and start playing.
        source <- genObjectName
        buffer source $= Just buf
        play [source]
        -- Normally nothing should go wrong above, but one never knows...
        errs <- get alErrors
        unless (null errs) $ do
            hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
            --exitFailure
        return ()

playSounds :: SoundSignal -> IO ()
playSounds (SoundSignal True _) = playSound "sounds/rising.wav"
playSounds (SoundSignal _    _) = return ()
