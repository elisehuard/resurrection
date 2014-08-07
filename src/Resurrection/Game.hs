{-# LANGUAGE PackageImports, RecursiveDo #-}
module Resurrection.Game where

import FRP.Elerea.Param
import Resurrection.Types
import Resurrection.Graphics
import Resurrection.FRP
import Graphics.Rendering.OpenGL hiding (Front)
import qualified Graphics.Rendering.FTGL as FTGL
import Control.Applicative hiding (Const)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Debug.Trace

-- initial player position
playerPos0 = Vector2 200 200
initialPlayer = Player { pos = playerPos0, direction = Front, action = Neutral, tick =  0 }
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
resurrection windowSize textures font window = mdo
  let initialGameState = Between 1
      startLevel level = playLevel window level

  (levelState, levelTrigger) <- switcher (startLevel <$> levelCount)
  levelTrigger' <- delay False levelTrigger
  levelCount <- transfer 1 (\dt success current -> if success then current+1 else current) levelTrigger'
  return $ (renderFrame textures font window) <$> windowSize <*> levelState



playLevel :: Window -> Int -> SignalGen Double (Signal LevelState, Signal Bool)
playLevel window levelCount = mdo 
                                              directionControl <- effectful $ (,,,)
                                                <$> keyIsPressed window Key'Left
                                                <*> keyIsPressed window Key'Up
                                                <*> keyIsPressed window Key'Down
                                                <*> keyIsPressed window Key'Right
                                              resurrectControl <- effectful $ keyIsPressed window Key'Space
                                              killControl <- effectful $ keyIsPressed window Key'K

                                              -- life evolution todo
                                              --    when resurrecting something:
                                              --    decrement life until either the thing is resurrected or the player dies
                                              -- player: initialPlayer in function of level? or continuation of upper level stuff?
                                              player <- transfer initialPlayer (\dt keyP p -> updateFromKey p keyP) directionControl
                                              player' <- delay initialPlayer player

                                              -- signal containing resurrections: used to calculate cost to life and also evolution of the world
                                              resurrections <- transfer3 (False, []) (activated resurrectScope) player' resurrectControl world'
                                              resurrections' <- delay (False, []) resurrections

                                              killing <- transfer3 (False, []) (activated killScope) player' killControl world'
                                              killing' <- delay (False, []) killing

                                              -- todo: initialLife in function of level? or continuation of upper level stuff?
                                              life <- transfer2 20 lifeAccounting resurrections' killing'
                                              life' <- delay 20 life

                                              -- state of the world
                                              -- todo: initialWorld in function of level
                                              world <- transfer3 (initialWorld levelCount) (\dt p r k w -> worldEvolution dt p r k w) player' resurrectControl killControl
                                              world' <- delay (initialWorld levelCount) world

                                              return ( LevelState levelCount <$> world <*> player <*> life -- level state
                                                      , goalAchieved levelCount <$> world <*> player <*> life) -- goal achieved in level

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

-- Data per level
--   - goal to be achieved
--   - initial world
goalAchieved levelCount (World _ lifeforms) _ _ = all alive lifeforms

initialWorld 1 = World (Background (Level 1) (640, 480)) [Lifeform (Vector2 300 300) Grass Dead 5]
initialWorld 2 = World (Background (Level 1) (640, 480)) [Lifeform (Vector2 200 200) Grass Dead 5, Lifeform (Vector2 400 400) Grass Dead 5]
