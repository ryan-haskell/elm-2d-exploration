module Elm2d exposing
    ( SimpleProgram, view
    , Program, View, program
    , Angle, Camera, Color, Node, SpriteSheet, Transform, Vector
    )

{-|

@docs SimpleProgram, view
@docs Program, View, program

@docs Angle, Camera, Color, Node, SpriteSheet, Transform, Vector

-}

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Elm2d.Angle
import Elm2d.Camera as Camera
import Elm2d.Color as Color
import Elm2d.Internals.Node as Node
import Elm2d.Internals.Response as Response exposing (Response)
import Elm2d.Internals.SpriteSheet as SpriteSheet
import Elm2d.Transform
import Elm2d.Vector
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Task
import WebGL
import WebGL.Texture as Texture exposing (Texture)


type alias SimpleProgram =
    Program () () ()


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


type alias SpriteSheet =
    SpriteSheet.SpriteSheet


type alias Node msg =
    Node.Node msg


type alias Color =
    Color.Color


type alias Camera =
    Camera.Camera


type alias Transform =
    Elm2d.Transform.Transform


type alias Vector =
    Elm2d.Vector.Vector


type alias Angle =
    Elm2d.Angle.Angle


view : View () -> SimpleProgram
view theView =
    program
        { init = \_ -> ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> theView
        }


program :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> View msg
    }
    -> Program flags model msg
program props =
    Browser.element
        { init = makeInit props
        , update = makeUpdate props.update
        , subscriptions = makeSubscriptions props.subscriptions
        , view = makeView props.view
        }



-- THE ELM ARCHITECTURE


type alias Model model =
    { window : Maybe ( Int, Int )
    , game : model
    , textures : Dict String (Response Texture.Error Texture)
    }


type alias View msg =
    { camera : Camera
    , background : Color
    , nodes : List (Node msg)
    }


findTextureUrls : View msg -> List String
findTextureUrls view_ =
    view_.nodes
        |> List.concatMap Node.findTextureUrls
        |> List.Extra.unique


type Msg msg
    = Game msg
    | WindowResized Int Int
    | LoadedTexture String (Result Texture.Error Texture)


makeInit :
    { props | init : flags -> ( model, Cmd msg ), view : model -> View msg }
    -> flags
    -> ( Model model, Cmd (Msg msg) )
makeInit props flags =
    let
        ( gameModel, gameCmd ) =
            props.init flags

        textureUrlsToLoad : List String
        textureUrlsToLoad =
            findTextureUrls (props.view gameModel)
    in
    ( { game = gameModel
      , window = Nothing
      , textures = Dict.empty
      }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform
                (\{ viewport } ->
                    WindowResized
                        (floor viewport.width)
                        (floor viewport.height)
                )
        , Cmd.map Game gameCmd
        , textureUrlsToLoad
            |> List.map fetchTexture
            |> Cmd.batch
        ]
    )


fetchTexture : String -> Cmd (Msg msg)
fetchTexture url =
    Texture.loadWith
        { magnify = Texture.nearest
        , minify = Texture.nearest
        , horizontalWrap = Texture.clampToEdge
        , verticalWrap = Texture.clampToEdge
        , flipY = False
        }
        url
        |> Task.attempt (LoadedTexture url)


makeUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
makeUpdate update msg model =
    case msg of
        Game gameMsg ->
            let
                ( gameModel, gameCmd ) =
                    update gameMsg model.game
            in
            ( { model | game = gameModel }
            , Cmd.map Game gameCmd
            )

        WindowResized width height ->
            ( { model | window = Just ( width, height ) }
            , Cmd.none
            )

        LoadedTexture url result ->
            ( { model | textures = Dict.insert url (Response.fromResult result) model.textures }
            , Cmd.none
            )


makeSubscriptions : (model -> Sub msg) -> Model model -> Sub (Msg msg)
makeSubscriptions subscriptions model =
    Sub.batch
        [ Sub.map Game (subscriptions model.game)
        , Browser.Events.onResize WindowResized
        ]


makeView : (model -> View msg) -> Model model -> Html (Msg msg)
makeView toView model =
    let
        view_ : View msg
        view_ =
            toView model.game
    in
    case model.window of
        Nothing ->
            Html.text ""

        Just ( windowWidth, windowHeight ) ->
            Html.div
                [ Html.Attributes.style "width" (px windowWidth)
                , Html.Attributes.style "height" (px windowHeight)
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "background-color" "black"
                , Html.Attributes.style "overflow" "hidden"
                , Html.Attributes.style "image-rendering" "pixelated"
                ]
                [ WebGL.toHtmlWith
                    [ WebGL.alpha True
                    , WebGL.antialias
                    , Color.toWebGlClearColor view_.background
                    ]
                    (Camera.toCanvasAttributes ( windowWidth, windowHeight ) view_.camera)
                    (List.map
                        (Node.toWebGLEntity
                            { camera = view_.camera
                            , getTexture =
                                \url ->
                                    Dict.get url model.textures
                                        |> Maybe.andThen Response.toMaybe
                            }
                        )
                        view_.nodes
                    )
                ]



-- HELPERS


px : Int -> String
px value =
    String.fromInt value ++ "px"
