module Gamepad.Demo where

import Relude
import System.Environment
import Gamepad.App.Type (App)
import Gamepad.App.Run (runApp)
import qualified Gamepad.Demo.BlankWindow
import qualified Gamepad.Demo.Triangle

main :: IO ()
main = lookupEnv "demo" >>=
  \case
    Just x -> f x
    Nothing -> runApp defaultDemo

f :: String -> IO ()
f "blank window" = runApp Gamepad.Demo.BlankWindow.app
f "triangle" = runApp Gamepad.Demo.Triangle.app
f _ = putStrLn "?"

defaultDemo :: App
defaultDemo = Gamepad.Demo.Triangle.app
