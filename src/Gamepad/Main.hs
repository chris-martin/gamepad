module Gamepad.Main where

import Graphics.UI.GLUT
import Relude

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    createWindow "Gamepad"
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)

    reshapeCallback $= Just \s@(Size width height) -> do
        let half z = realToFrac z / 2
            w2 = half width
            h2 = half height
        viewport   $= (Position 0 0, s)
        matrixMode $= Projection
        loadIdentity
        perspective 45 (w2/h2) 1 1000
        matrixMode $= Modelview 0

    displayCallback $= do
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        translate $ Vector3 @GLfloat 0 0 (-80)
        rotate @GLfloat (3 * 10) (Vector3 1 0 0)
        color $ Color3 @GLfloat 1 1 0.2
        position (Light 0) $= Vertex4 0 0 0 1
        renderObject Wireframe (Cube 40)
        swapBuffers

    mainLoop
