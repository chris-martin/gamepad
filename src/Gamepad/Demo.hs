module Gamepad.Demo where

import Relude
import System.Environment
import Gamepad.App.Run (runApp)
import qualified Gamepad.Demo.BlankWindow

main :: IO ()
main = getEnv "demo" >>=
  \case
    "blank window" -> runApp Gamepad.Demo.BlankWindow.app
