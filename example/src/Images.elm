module Images exposing (main)

import Elm2d
import Elm2d.Camera as Camera
import Elm2d.Color as Color
import Elm2d.Node as Node
import Elm2d.Vector as Vector


main : Elm2d.SimpleProgram
main =
    Elm2d.view
        { camera = Camera.integerScaling 100 75
        , background = Color.white
        , nodes =
            [ Node.image
                { url = "/assets/Cute_Fantasy_Free/Player/Player.png"
                , size = ( 31, 31 )
                , offset = ( 0, 0 )
                }
            ]
        }
