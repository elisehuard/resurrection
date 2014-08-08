module Resurrection.Types where

import Graphics.Rendering.OpenGL hiding (Level)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

data LifeStatus = Alive | Dead | Resurrecting Int | Dying Int
                  deriving Show

type Life = Int

data Background = Background Level (GLdouble, GLdouble)
                  deriving Show

data Species = Grass
               deriving Show

data Lifeform = Lifeform (Vector2 GLdouble) Species LifeStatus Life
                deriving Show

data World = World Background [Lifeform]
             deriving Show

data Player = Player {
                pos :: Vector2 GLdouble,
                direction :: Direction,
                action :: Action,
                tick :: Double
              }
              deriving Show

data Direction = Front | GoBack | GoLeft | GoRight
                 deriving Show

data Action = Neutral | Walking
              deriving Show

type Textures = Map.Map String (Maybe TextureObject)

data Level = Between Int | Level Int
             deriving Show

data LevelState = LevelState { level :: Level
                             , world :: World
                             , player :: Player
                             , life :: Life
                             , success :: Bool } |
                  InBetweenState Level World

-- transfer of data related to sounds
data SoundSignal = SoundSignal { res :: Bool,
                               killing :: Bool }

-- passing in textures as reader
class Draw a where
  draw :: a -> Textures -> IO ()
