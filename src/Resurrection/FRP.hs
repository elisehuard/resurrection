{-# LANGUAGE RecursiveDo #-}
module Resurrection.FRP 
( driveNetwork
, switcher)
where

import FRP.Elerea.Param

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import Data.Traversable hiding (sequence)

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

infix 2 -->

x0 --> s = transfer x0 store s
    where store _ Nothing  x = x
          store _ (Just x) _ = x

-- result: generated signal, bool
{-
switcher :: SignalGen (Signal (Signal b0, Signal Bool), Signal Bool) -> 
            ((Signal (Signal b0, Signal Bool), Signal Bool) -> SignalGen (Signal Bool))
            -> (Signal (IO ()), Signal Bool)
-}
-- gen  SignalGen Double (IO ())
-- generator goes Signal (SignalGen p a) -> SignalGen p (Signal a)
switcher gen = mdo
  trig <- memo (snd =<< pw)
  trig' <- delay True trig -- signal bool
  ss <- generator (toMaybe <$> trig' <*> gen) -- signal of generators to run
  pw <- undefined --> ss -- signal that starts at undefined and progresses on path of generator
  return (fst =<< pw,trig)

toMaybe b s = if b then Just <$> s else pure Nothing
