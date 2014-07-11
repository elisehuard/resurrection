module Resurrection.FRP (driveNetwork)
where

import FRP.Elerea.Param

import Control.Monad (join)

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()
