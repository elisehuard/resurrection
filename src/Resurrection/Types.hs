module Resurrection.Types where

import Graphics.Rendering.OpenGL
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

data GameState = Menu | Level Int
                 deriving Show

data LifeStatus = Alive | Dead
                  deriving Show

data Background = Background GameState (GLdouble, GLdouble)
                  deriving Show

data Species = Grass
               deriving Show
data Lifeform = Lifeform (Vector2 GLdouble) Species LifeStatus
                deriving Show

data World = World Background [Lifeform]
             deriving Show

data Player = Player (Vector2 GLdouble) Direction
              deriving Show

data Direction = Neutral | GoBack | GoLeft | GoRight
                 deriving Show

type Textures = Map.Map String (Maybe TextureObject)

-- passing in textures as reader
class Draw a where
  draw :: a -> Textures -> IO ()
