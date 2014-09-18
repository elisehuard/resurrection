{-# LANGUAGE PackageImports, RecursiveDo #-}
module Resurrection.Game where

import FRP.Elerea.Param
import Resurrection.Types
import Resurrection.Graphics
import Resurrection.FRP
import Resurrection.Sound
import Graphics.Rendering.OpenGL hiding (Front, Level)
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

outputFunction :: Textures -> FTGL.Font -> Window -> (GLdouble, GLdouble) -> LevelState -> SoundSignal -> IO ()
outputFunction textures font window windowSize levelState soundSignals =  (renderFrame textures font window windowSize levelState) >> (playSounds soundSignals)

resurrection :: Signal (GLdouble, GLdouble) -> Textures -> FTGL.Font -> Window -> Signal (Bool, Bool, Bool, Bool)
             -> Signal Bool -> Signal Bool -> Signal Bool -> SignalGen Double (Signal (IO())) 
resurrection windowSize textures font window directionKey resurrectKey killKey nextKey = mdo
  let initialLevel = Between 1
      startLevel dk rk kk nk level = playLevel window level dk rk kk nk

  (levelState, soundState, levelTrigger) <- levelGen (startLevel directionKey resurrectKey killKey nextKey <$> levelCount)
  levelTrigger' <- delay False levelTrigger
  levelCount <- transfer initialLevel levelProgression levelTrigger'
  return $ (outputFunction textures font window) <$> windowSize <*> levelState <*> soundState


playLevel :: Window -> Level -> Signal (Bool, Bool, Bool, Bool) -> Signal Bool -> Signal Bool -> Signal Bool -> SignalGen Double (Signal LevelState, Signal SoundSignal, Signal Bool)
playLevel window level@(Level _) directionControl resurrectControl killControl nextControl = mdo
                                -- life evolution todo
                                --    when resurrecting something:
                                --    decrement life until either the thing is resurrected or the player dies
                                player <- transfer initialPlayer (\dt keyP p -> updateFromKey p keyP) directionControl
                                player' <- delay initialPlayer player

                                -- signal containing resurrections: used to calculate cost to life and also evolution of the world
                                resurrections <- transfer3 (False, []) (activated resurrectScope) player' resurrectControl world'
                                resurrections' <- delay (False, []) resurrections

                                killing <- transfer3 (False, []) (activated killScope) player' killControl world'
                                killing' <- delay (False, []) killing


                                -- state of the world
                                -- todo: initialWorld in function of level
                                world <- transfer4 (initialWorld level) (\dt p r k f w -> worldEvolution level dt p r k f w) player' resurrectControl killControl failed
                                world' <- delay (initialWorld level) world

                                -- todo: initialLife in function of level? or continuation of upper level stuff?
                                life <- transfer3 20 lifeAccounting resurrections' killing' failed
                                life' <- delay 20 life

                                let success = goalAchieved level <$> world' <*> player' <*> life'

                                trackLevelStage <- transfer3 (Introduction, 0) (\dt next succeeded l stage -> transition dt stage next succeeded l) nextControl success life'
                                trackLevelStage' <- delay (Introduction, 0) trackLevelStage
                                let levelStage = fst <$> trackLevelStage'
                                    completed = (== End) <$> levelStage
                                    failed = (== Fail) <$> levelStage

                                return ( LevelState level <$> levelStage <*> world <*> player <*> life <*> success -- level state
                                        , soundSignal <$> resurrections <*> killing -- notifications for sound
                                        , liftA2 (&&) completed nextControl ) -- goal achieved in level - move on to next
                                where soundSignal (boolRes, _) (boolKill, _) = SoundSignal boolRes boolKill

-- player only needs to indicate when wants to progress
playLevel window level@(Between _) directionControl resurrectControl killControl nextControl = mdo
                                -- signal to avoid passing to next level as soon as reached
                                active <- activeSignal
                                fadeIn <- stateful 0 (\dt previous -> cappedIncrease previous)
                                let state = InBetweenState level (inBetweenWorld level)
                                    sound = SoundSignal False False
                                return ( state <$> fadeIn
                                    , pure sound
                                    , (&&) <$> nextControl <*> active )
                                where cappedIncrease num
                                        | num < 1 = num + 0.005
                                        | otherwise = 1

-- signal only pops up value when starting resurrection of a lifeform

activated scope dt (Player position _ _ _) True (World _ lifeforms) (False, previousResurrect) = (True, scope lifeforms position)
activated scope dt (Player position _ _ _) True (World _ lifeforms) (True, previousResurrect) = (True, [])
activated scope dt _                       False _                  _ = (False, [])

tally lifeforms = foldr addLifeformCost 0 lifeforms

addLifeformCost (Lifeform _ _ _ cost) currentCount = currentCount + cost

lifeAccounting dt (_, resurrections) (_, killings) False currentLife = currentLife - (tally resurrections) + (tally killings)
lifeAccounting dt _ _ True _ = 20 -- reinitialize on fail

resurrectScope lifeforms position = filter (\l ->  dead l && colliding l position) lifeforms
killScope lifeforms position = filter (\l ->  alive l && colliding l position) lifeforms

dead (Lifeform _ _ Dead _) = True
dead (Lifeform _ _ _    _) = False
alive (Lifeform _ _ Alive _) = True
alive (Lifeform _ _ _    _) = False

rabbits (Lifeform _ Rabbit _ _) = True
rabbits (Lifeform _ _ _ _) = False

-- remove any lifeforms that haven't been completely resurrected -- todo when using resurrecting and dying status
-- completed lifeforms = lifeforms

-- if player stands on lifeform, and lifeform is dead, and they press space (for resurrect), then the lifeform resurrects
-- todo next step: points accounting (lifeform cost + player life down)
-- todo: evolution of the life going from player to thing + way to represent this (text first step?)
worldEvolution _ dt (Player position _ _ _) True False False (World background lifeforms) = World background (resurrect dt lifeforms position)
worldEvolution _ dt (Player position _ _ _) False True False (World background lifeforms) = World background (kill dt lifeforms position)
worldEvolution level dt (Player position _ _ _) False True True  _ = initialWorld level -- reinitialize on fail
worldEvolution _ dt (Player position _ _ _) True True   _ world                        = world -- confused player
worldEvolution _ dt _                       False False _ world                        = world

resurrect dt lifeforms position = map (\lifeform -> resurrectColliding (colliding lifeform position) lifeform) lifeforms
kill dt lifeforms position = map (\lifeform -> killColliding (colliding lifeform position) lifeform) lifeforms

-- for now, but accounting will change this.
resurrectColliding True (Lifeform pos species _ cost) = Lifeform pos species Alive cost
resurrectColliding False lifeform = lifeform
killColliding True (Lifeform pos species _ cost) = Lifeform pos species Dead cost
killColliding False lifeform = lifeform

-- Data per level: parse from file when established what's needed
--   - goal to be achieved
--   - initial world
--   - explanatory text
goalAchieved a w c d | trace ("goalachieved " ++ show w) False = undefined
goalAchieved (Level 1) (World _ lifeforms) _ _ = all alive lifeforms
goalAchieved (Level 2) (World _ lifeforms) _ _ = any (\l -> alive l && rabbits l) lifeforms

inBetweenWorld (Between 1) = World (Background (Between 1) (640, 480)) []
inBetweenWorld (Between 2) = World (Background (Between 1) (640, 480)) []

initialWorld (Level 1) = World (Background (Level 1) (640, 480)) [Lifeform (Vector2 300 300) Grass Dead 5]
initialWorld (Level 2) = World (Background (Level 2) (640, 480)) [Lifeform (Vector2 200 200) Grass Alive 5, Lifeform (Vector2 400 400) Grass Alive 5, Lifeform (Vector2 100 100) Rabbit Dead 25]

-- boolean indicates whether goal of level was achieved
levelProgression _ False current = current
levelProgression _ True (Level n) = Level (n + 1)
levelProgression _ True (Between n) = Level n

-- transition of level stage
-- (\dt next succeeded stage life -> transition stage next succeeded life)
-- also return the time since last transition - no transitions right after arriving into state
-- if life dips below 0: fail!
transition dt (Introduction, startTime) False _ _ = (Introduction, startTime + dt)
transition dt (Introduction, startTime) True  _ _
   | startTime > 1 = (FadeIn 1, 0)
   | startTime < 1 = (Introduction, startTime + dt)
transition dt (FadeIn n, startTime) _ _ _
   | n > 0 = (FadeIn (n - 0.01), startTime + dt)
   | n <= 0 = (Play, 0)
transition dt (Play, startTime) _ False life
   | life > 0 = (Play, startTime + dt)
   | life <= 0 = (FadeOutFail 0, 0)
transition _ (Play, startTime) _ True life -- end with positive life!
   | life > 0 = (FadeOut 0, 0)
   | life <= 0 = (FadeOutFail 0, 0)
transition dt (FadeOut n, startTime) _ _ life
   | life <= 0 = (FadeOutFail 0, 0)
   | startTime < 1 = (FadeOut 0, startTime + dt)
   | startTime >= 1 && startTime < 2 = (FadeOut (n + 0.01), startTime + dt)
   | startTime >= 2 = (End, 0)
transition dt (End, startTime) _ _ _ = (End, startTime + dt)
transition dt (FadeOutFail n, startTime) _ _ _
   | startTime < 1 = (FadeOutFail 0, startTime + dt)
   | startTime >= 1 && startTime < 2 = (FadeOutFail (n + 0.01), startTime + dt)
   | startTime >= 2 = (Fail, 0)
transition dt (Fail, startTime) True _ _ = (FadeIn 0, startTime)
transition dt (Fail, startTime) False _ _ = (Fail, startTime)

-- wait a second so that enter is not triggered immediately when transitioning
activeSignal = do
              accum <- stateful 0 (+)
              transfer False (\dt a b -> b || (a > 1)) accum
