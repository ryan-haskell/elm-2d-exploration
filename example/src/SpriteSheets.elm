module SpriteSheets exposing (main)

import Browser.Events
import Elm2d
import Elm2d.Angle as Angle
import Elm2d.Camera as Camera
import Elm2d.Color as Color
import Elm2d.Node as Node
import Elm2d.SpriteSheet as SpriteSheet
import Elm2d.Vector as Vector


main : Elm2d.Program () Model Msg
main =
    Elm2d.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { frame : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { frame = 0 }
    , Cmd.none
    )


type Msg
    = Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame ->
            ( { model | frame = model.frame + 1 }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (always Frame)


view : Model -> Elm2d.View Msg
view model =
    { camera = Camera.integerScaling 96 128
    , background = Color.rgb 50 200 150
    , nodes =
        [ Node.sprite
            { spriteSheet = spriteSheet
            , animation = "idle_right"
            , frame = model.frame // 8
            }
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "attack_right"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 32 0)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "walk_right"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 64 0)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "idle_down"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 0 32)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "attack_down"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 32 32)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "walk_down"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 64 32)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "idle_up"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 0 64)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "attack_up"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 32 64)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "walk_up"
            , frame = model.frame // 8
            }
            |> Node.move (Vector.new 64 64)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "idle_right"
            , frame = model.frame // 8
            }
            |> Node.flipX
            |> Node.move (Vector.new 0 96)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "attack_right"
            , frame = model.frame // 8
            }
            |> Node.flipX
            |> Node.move (Vector.new 32 96)
        , Node.sprite
            { spriteSheet = spriteSheet
            , animation = "walk_right"
            , frame = model.frame // 8
            }
            |> Node.flipX
            |> Node.move (Vector.new 64 96)
        ]
    }


spriteSheet : Elm2d.SpriteSheet
spriteSheet =
    SpriteSheet.new
        { url = "/assets/Cute_Fantasy_Free/Player/Player.png"
        , size = ( 31, 31 )
        }
        |> SpriteSheet.withGap ( 1, 1 )
        |> SpriteSheet.withAnimation "idle_down" [ 0, 1, 2, 3, 4, 5 ]
        |> SpriteSheet.withAnimation "idle_right" [ 6, 7, 8, 9, 10, 11 ]
        |> SpriteSheet.withAnimation "idle_up" [ 12, 13, 14, 15, 16, 17 ]
        |> SpriteSheet.withAnimation "walk_down" [ 18, 19, 20, 21, 22, 23 ]
        |> SpriteSheet.withAnimation "walk_right" [ 24, 25, 26, 27, 28, 29 ]
        |> SpriteSheet.withAnimation "walk_up" [ 30, 31, 32, 33, 34, 35 ]
        |> SpriteSheet.withAnimation "attack_down" [ 36, 37, 38, 39 ]
        |> SpriteSheet.withAnimation "attack_right" [ 42, 43, 44, 45 ]
        |> SpriteSheet.withAnimation "attack_up" [ 48, 49, 50, 51 ]
        |> SpriteSheet.withAnimation "death" [ 54, 55, 56, 57 ]
