# @ryan-haskell/elm-2d
> A package for making 2D games and apps in Elm.

## __Installation__

```bash
elm install ryan-haskell/elm-2d
```

## __Usage__

```elm
module Main exposing (main)

import Elm2d
import Elm2d.Events as Events
import Elm2d.Vector as Vector
import Elm2d.Input as Input
import Elm2d.Camera as Camera
import Elm2d.Color as Color
import Elm2d.Node as Node

main : Elm2d.Program
main =
    Elm2d.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { input : Elm2d.Vector
    , position : Elm2d.Vector
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = Vector.zero
      , position = Vector.zero
      }
    , Cmd.none
    )

type Msg
    = PlayerMoved Elm2d.Vector
    | Frame Float

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerMoved newInput ->
            ( { model | input = newInput }
            , Cmd.none
            )

        Frame dt ->
            ( { model
                | position =
                    model.position
                        |> Vector.add (Vector.scale dt model.input)
                        |> Vector.clamp ( -150, -100 ) ( 150, 100 )
            }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Input.onAxis PlayerMoved
            { x = ( "A", "D" )
            , y = ( "W", "S" )
            }
        , Events.onFrame Frame
        ]

view : Model -> Elm2d.View Msg
view model =
    { camera = Camera.fixed 400 300
    , background = Color.black
    , nodes =
        [ Node.image
            { url = "/assets/player.png"
            , size = ( 50, 50 )
            }
            |> Node.setPosition model.position
        , Node.image
            { url = "/assets/tree.png"
            , size = ( 50, 50 )
            }
            |> Node.setPosition (Vector.new 100 100)
        ]
    }
```

## License

BSD-3-Clause
