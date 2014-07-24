{-# LANGUAGE PackageImports, RecursiveDo #-}
module Resurrection.Game where

import FRP.Elerea.Param
import Resurrection.Types
import Resurrection.Graphics
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import Control.Applicative hiding (Const)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Debug.Trace

-- initial player position
playerPos0 = Vector2 200 200
initialPlayer = Player playerPos0 Neutral
initialWorld = World (Background (Level 1) (640, 480)) [Lifeform (Vector2 300 300) Grass Dead 5]
initialLife = 20

-- TODO limits of the world - go round?
updateFromKey :: Player -> (Bool, Bool, Bool, Bool) -> Player
updateFromKey (Player (Vector2 x y) _) keyP = case keyP of -- todo: all keys pressed considered?
                                     (True, _, _, _)  -> Player (Vector2 (x - 5) y) GoLeft
                                     (_, True, _, _)   -> Player (Vector2 x (y + 5)) GoBack
                                     (_, _, True, _)  -> Player (Vector2 x (y - 5)) Neutral
                                     (_, _, _, True) -> Player (Vector2 (x + 5) y) GoRight
                                     otherwise    -> Player (Vector2 x y) Neutral

resurrection :: Signal (GLdouble, GLdouble) -> Textures -> Font -> Window -> SignalGen Double (Signal (IO())) 
resurrection windowSize textures font window  = mdo 
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

                                              -- life evolution todo
                                              --    when resurrecting something:
                                              --    decrement life until either the thing is resurrected or the player dies
                                              --life <- transfer initialLife evolveLife resurrectControl
                                              player <- transfer initialPlayer (\dt keyP p -> updateFromKey p keyP) directionControl
                                              player' <- delay initialPlayer player

                                              -- signal containing resurrections: used to calculate cost to life and also evolution of the world
                                              resurrections <- transfer3 (False, []) resurrecting player' resurrectControl world'
                                              resurrections' <- delay (False, []) resurrections

                                              life <- transfer 20 lifeAccounting resurrections'
                                              life' <- delay 20 life

                                              -- state of the world

                                              world <- transfer2 initialWorld (\dt p r w -> worldEvolution dt p r w) player' resurrectControl
                                              world' <- delay initialWorld world

                                              return $ (renderLevel textures font window) <$> windowSize <*> world <*> player <*> life <*> fpsTracking


-- signal only pops up value when starting resurrection of a lifeform

resurrecting dt (Player position _) True (World _ lifeforms) (False, previousResurrect) = (True, resurrectScope lifeforms position)
resurrecting dt (Player position _) True (World _ lifeforms) (True, previousResurrect) = (True, [])
resurrecting dt _                   False _                  _ = (False, [])

tally lifeforms = foldr addLifeformCost 0 lifeforms

addLifeformCost (Lifeform _ _ _ cost) currentCount = currentCount + cost

lifeAccounting a b c | trace ("accounting" ++ show b ) False = undefined
lifeAccounting dt (True, resurrections) currentLife = currentLife - (tally resurrections)
lifeAccounting dt (False, resurrections) currentLife = currentLife - (tally resurrections)

resurrectScope lifeforms position = filter (\l ->  dead l && colliding l position) lifeforms

dead (Lifeform _ _ Dead _) = True
dead (Lifeform _ _ _    _) = False

-- remove any lifeforms that haven't been completely resurrected -- todo when using resurrecting and dying status
-- completed lifeforms = lifeforms

-- if player stands on lifeform, and lifeform is dead, and they press space (for resurrect), then the lifeform resurrects
-- todo next step: points accounting (lifeform cost + player life down)
-- todo: evolution of the life going from player to thing + way to represent this (text first step?)
worldEvolution dt (Player position _) True (World background lifeforms) = World background (resurrect dt lifeforms position)
worldEvolution dt _                   False world                       = world

resurrect dt lifeforms position = map (\lifeform -> resurrectColliding (colliding lifeform position) lifeform) lifeforms

-- for now, but accounting will change this.
resurrectColliding True (Lifeform pos species _ cost) = Lifeform pos species Alive cost
resurrectColliding False lifeform = lifeform
