module Gamepad.App.Run where

import qualified Graphics.UI.GLUT as GLUT
import Data.StateVar (($=))
import Relude
import Gamepad.App.Type (App (App))
import qualified Gamepad.App.Type as App
import Graphics.UI.GLUT.Objects

runApp :: App -> IO ()
runApp App{ windowTitle } =
  do
    _ <- GLUT.getArgsAndInitialize
    _ <- GLUT.createWindow (toString windowTitle)
    GLUT.displayCallback $=
      do
        renderObject Wireframe (Cube 20)
    GLUT.mainLoop
