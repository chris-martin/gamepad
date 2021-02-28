module Gamepad.Demo where

import Relude
import System.Environment
import Gamepad.App.Run (runApp)
import qualified Gamepad.Demo.BlankWindow

main :: IO ()
main = lookupEnv "demo" >>=
  \case
    Just x -> f x
    Nothing -> runApp defaultDemo

f "blank window" = runApp Gamepad.Demo.BlankWindow.app

defaultDemo = Gamepad.Demo.BlankWindow.app
