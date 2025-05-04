module Elm2d.Camera exposing
    ( Camera
    , fixed, integerScaling, floatScaling
    , toCanvasAttributes, toMatrix, toAspectRatio
    )

{-|

@docs Camera
@docs fixed, integerScaling, floatScaling

@docs toCanvasAttributes, toMatrix, toAspectRatio

-}

import Elm2d.Vector as Vector exposing (Vector)
import Html
import Html.Attributes
import Math.Matrix4
import Math.Vector3


type Camera
    = Fixed Int Int
    | IntegerScaled Int Int
    | FloatScaled Int Int


fixed : Int -> Int -> Camera
fixed width height =
    Fixed width height


integerScaling : Int -> Int -> Camera
integerScaling width height =
    IntegerScaled width height


floatScaling : Int -> Int -> Camera
floatScaling width height =
    FloatScaled width height



-- READING VALUES


toAspectRatio : Camera -> Float
toAspectRatio camera =
    case camera of
        Fixed width height ->
            toFloat width / toFloat height

        IntegerScaled width height ->
            toFloat width / toFloat height

        FloatScaled width height ->
            toFloat width / toFloat height


toCanvasAttributes : ( Int, Int ) -> Camera -> List (Html.Attribute msg)
toCanvasAttributes ( windowWidth, windowHeight ) camera =
    case camera of
        Fixed width height ->
            [ Html.Attributes.width width
            , Html.Attributes.height height
            ]

        IntegerScaled width height ->
            let
                widthMultiples =
                    windowWidth // width

                heightMultiples =
                    windowHeight // height
            in
            if width > windowWidth || height > windowHeight then
                [ Html.Attributes.width width
                , Html.Attributes.height height
                ]

            else if widthMultiples < heightMultiples then
                [ Html.Attributes.width width
                , Html.Attributes.height height
                , Html.Attributes.style "scale" (String.fromInt widthMultiples)
                ]

            else
                [ Html.Attributes.width width
                , Html.Attributes.height height
                , Html.Attributes.style "scale" (String.fromInt heightMultiples)
                ]

        FloatScaled width height ->
            let
                ( fWidth, fHeight ) =
                    ( toFloat width, toFloat height )

                ( fWindowWidth, fWindowHeight ) =
                    ( toFloat windowWidth, toFloat windowHeight )

                widthMultiples =
                    fWindowWidth / fWidth

                heightMultiples =
                    fWindowHeight / fHeight
            in
            if widthMultiples < heightMultiples then
                [ Html.Attributes.width width
                , Html.Attributes.height height
                , Html.Attributes.style "scale" (String.fromFloat widthMultiples)
                ]

            else
                [ Html.Attributes.width width
                , Html.Attributes.height height
                , Html.Attributes.style "scale" (String.fromFloat heightMultiples)
                ]


toMatrix : Camera -> Math.Matrix4.Mat4
toMatrix camera =
    case camera of
        Fixed width height ->
            Math.Matrix4.makeOrtho2D 0 (toFloat width) (toFloat height) 0

        IntegerScaled width height ->
            Math.Matrix4.makeOrtho2D 0 (toFloat width) (toFloat height) 0

        FloatScaled width height ->
            Math.Matrix4.makeOrtho2D 0 (toFloat width) (toFloat height) 0



-- let
--     scale =
--         2 / camera.height
-- in
-- Math.Matrix4.makeLookAt
--     (Math.Vector3.vec3
--         (Vector.x camera.center)
--         (Vector.y camera.center)
--         0
--     )
--     (Math.Vector3.vec3 0 0 1)
--     (Math.Vector3.vec3 0 -1 0)
--     |> Math.Matrix4.scale3 (scale / aspectRatio) scale 1
