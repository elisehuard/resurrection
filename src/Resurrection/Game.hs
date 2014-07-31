{-# LANGUAGE PackageImports, RecursiveDo #-}
module Resurrection.Game where

import FRP.Elerea.Param
import Resurrection.Types
import Resurrection.Graphics
import Graphics.Rendering.OpenGL hiding (Front)
import qualified Graphics.Rendering.FTGL as FTGL
import Control.Applicative hiding (Const)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Debug.Trace

-- initial player position
playerPos0 = Vector2 200 200
initialPlayer = Player { pos = playerPos0, direction = Front, action = Neutral, tick =  0 }
initialWorld = World (Background (Level 1) (640, 480)) [Lifeform (Vector2 300 300) Grass Dead 5]
initialLife = 20

-- TODO limits of the world - go round?
updateFromKey :: Player -> (Bool, Bool, Bool, Bool) -> Player
updateFromKey (Player (Vector2 x y) _ _ t) keyP = do let newTick = oscillating t
                                                     case keyP of -- todo: all keys pressed considered?
                                                         (True, _, _, _)  -> Player (Vector2 (x - 5) y) GoLeft Walking newTick
                                                         (_, True, _, _)   -> Player (Vector2 x (y + 5)) GoBack Walking newTick
                                                         (_, _, True, _)  -> Player (Vector2 x (y - 5)) Front Walking newTick
                                                         (_, _, _, True) -> Player (Vector2 (x + 5) y) GoRight Walking newTick
                                                         otherwise    -> Player (Vector2 x y) Front Neutral newTick

oscillating t
           | t == 2*pi  = 0
           | otherwise = t + pi/6

resurrection :: Signal (GLdouble, GLdouble) -> Textures -> FTGL.Font -> Window -> SignalGen Double (Signal (IO())) 
resurrection windowSize textures font window  = mdo 
                                              directionControl <- effectful $ (,,,)
                                                <$> keyIsPressed window Key'Left
                                                <*> keyIsPressed window Key'Up
                                                <*> keyIsPressed window Key'Down
                                                <*> keyIsPressed window Key'Right
                                              resurrectControl <- effectful $ keyIsPressed window Key'Space
                                              killControl <- effectful $ keyIsPressed window Key'K
                                              fpsTracking <- stateful (0, 0, Nothing) $ \dt (time, count, _) ->
                                                    let time' = time + dt
                                                        done = time > 5
                                                    in if done
                                                    then (0, 0, Just (count / time'))
                                                    else (time', count + 1, Nothing)

                                              -- life evolution todo
                                              --    when resurrecting something:
                                              --    decrement life until either the thing is resurrected or the player dies
                                              --life <- transfer initialLife evolveLife resurrectControl
                                              player <- transfer initialPlayer (\dt keyP p -> updateFromKey p keyP) directionControl
                                              player' <- delay initialPlayer player

                                              -- signal containing resurrections: used to calculate cost to life and also evolution of the world
                                              resurrections <- transfer3 (False, []) (activated resurrectScope) player' resurrectControl world'
                                              resurrections' <- delay (False, []) resurrections

                                              killing <- transfer3 (False, []) (activated killScope) player' killControl world'
                                              killing' <- delay (False, []) killing

                                              life <- transfer2 20 lifeAccounting resurrections' killing'
                                              life' <- delay 20 life

                                              -- state of the world
                                              world <- transfer3 initialWorld (\dt p r k w -> worldEvolution dt p r k w) player' resurrectControl killControl
                                              world' <- delay initialWorld world

                                              return $ (renderFrame textures font window) <$> windowSize <*> world <*> player <*> life <*> fpsTracking


-- signal only pops up value when starting resurrection of a lifeform

activated scope dt (Player position _ _ _) True (World _ lifeforms) (False, previousResurrect) = (True, scope lifeforms position)
activated scope dt (Player position _ _ _) True (World _ lifeforms) (True, previousResurrect) = (True, [])
activated scope dt _                       False _                  _ = (False, [])

tally lifeforms = foldr addLifeformCost 0 lifeforms

addLifeformCost (Lifeform _ _ _ cost) currentCount = currentCount + cost

lifeAccounting dt (_, resurrections) (_, killings) currentLife = currentLife - (tally resurrections) + (tally killings)

resurrectScope lifeforms position = filter (\l ->  dead l && colliding l position) lifeforms
killScope lifeforms position = filter (\l ->  alive l && colliding l position) lifeforms

dead (Lifeform _ _ Dead _) = True
dead (Lifeform _ _ _    _) = False
alive (Lifeform _ _ Alive _) = True
alive (Lifeform _ _ _    _) = False

-- remove any lifeforms that haven't been completely resurrected -- todo when using resurrecting and dying status
-- completed lifeforms = lifeforms

-- if player stands on lifeform, and lifeform is dead, and they press space (for resurrect), then the lifeform resurrects
-- todo next step: points accounting (lifeform cost + player life down)
-- todo: evolution of the life going from player to thing + way to represent this (text first step?)
worldEvolution dt (Player position _ _ _) True False  (World background lifeforms) = World background (resurrect dt lifeforms position)
worldEvolution dt (Player position _ _ _) False True  (World background lifeforms) = World background (kill dt lifeforms position)
worldEvolution dt (Player position _ _ _) True True   world                        = world -- confused player
worldEvolution dt _                       False False world                        = world

resurrect dt lifeforms position = map (\lifeform -> resurrectColliding (colliding lifeform position) lifeform) lifeforms
kill dt lifeforms position = map (\lifeform -> killColliding (colliding lifeform position) lifeform) lifeforms

-- for now, but accounting will change this.
resurrectColliding True (Lifeform pos species _ cost) = Lifeform pos species Alive cost
resurrectColliding False lifeform = lifeform
killColliding True (Lifeform pos species _ cost) = Lifeform pos species Dead cost
killColliding False lifeform = lifeform
