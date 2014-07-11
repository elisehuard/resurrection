module Resurrection.Graphics
(initCommon)
where

import Graphics.UI.GLUT hiding (position, scale)
import FRP.Elerea.Param

initCommon :: IO ()
initCommon = do
    _ <- getArgsAndInitialize

    initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
    rowAlignment Unpack $= 1

    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 100 100

    _ <- createWindow "Resurrection"
    -- actionOnWindowClose $= MainLoopReturns

    clearColor $= Color4 1 1 1 1

    shadeModel $= Flat
