module Gamepad.Demo.Triangle where

import qualified Gamepad.Demo.BlankWindow
import Gamepad.App.Type (App (App))
import qualified Gamepad.App.Type as App

app :: App
app = Gamepad.Demo.BlankWindow.app{ App.windowTitle = "Triangle" }
