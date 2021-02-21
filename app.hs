import Data.StateVar
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window
import Graphics.UI.GLUT.Callbacks.Window

main =
  do
    _ <- getArgsAndInitialize
    w <- createWindow "ey"
    displayCallback $= return ()
    mainLoop
