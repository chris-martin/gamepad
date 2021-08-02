module Gamepad.App.Run (runApp) where

import qualified Graphics.UI.GLUT as GLUT
import Relude
import Gamepad.App.Type (App (App))
import qualified Gamepad.App.Type as App
import Graphics.UI.GLUT
import qualified Foreign
import Foreign (Ptr)

vertexPositions :: [Vertex4 GLfloat]
vertexPositions =
    [ Vertex4   0.75    0.75  0 1
    , Vertex4   0.75  (-0.75) 0 1
    , Vertex4 (-0.75) (-0.75) 0 1
    ]

runApp :: App -> IO ()
runApp App{ windowTitle } =
  Foreign.allocaArray (3 * 4) $ \(vertexPositions :: Ptr GLfloat) ->
  do
    _ <- GLUT.getArgsAndInitialize
    _ <- GLUT.createWindow (toString windowTitle)

    Foreign.pokeArray vertexPositions
        [  0.75,  0.75, 0, 1
        ,  0.75, -0.75, 0, 1
        , -0.75, -0.75, 0, 1
        ]

    positionBufferObject <- genObjectName
    bindBuffer ArrayBuffer $= Just positionBufferObject
    bufferData ArrayBuffer $= (fromIntegral (Foreign.sizeOf vertexPositions), vertexPositions, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing

    displayCallback $=
      do

        -- The color that will be used when clearing the screen (black)
        clearColor $= Color4 0 0 0 0

        -- Clearing the color buffer sets everything to the clear color (black)
        clear [ColorBuffer]

        -- sets the current shader program to be used by all subsequent rendering commands
        -- glUseProgram(theProgram);

        -- These command set up the coordinates of the triangle to be rendered. They tell OpenGL the location in memory that the positions of the triangle will come from.
        bindBuffer ArrayBuffer $= _
        vertexAttribArray (AttribLocation 0) $= Enabled
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 4 Float 0 Foreign.nullPtr)

        -- uses the current state to generate a stream of vertices that will form triangles
        drawArrays Triangles 0 3

        -- cleanup
        vertexAttribArray (AttribLocation 0) $= Disabled
        -- glUseProgram(0);

        swapBuffers

    GLUT.mainLoop
