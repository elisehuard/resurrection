module Resurrection.Types where

import Graphics.Rendering.OpenGL
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

data GameState = Menu | Level Int

data LifeStatus = Alive | Dead

data Background = Background GameState (GLdouble, GLdouble)

data Species = Grass
data Lifeform = Lifeform (Vector2 GLdouble) Species LifeStatus

data Player = Player (Vector2 GLdouble) Direction

data Direction = Neutral | GoBack | GoLeft | GoRight

type Textures = Map.Map String (Maybe TextureObject)

-- passing in textures as reader
class Draw a where
  draw :: a -> Maybe TextureObject -> IO ()
