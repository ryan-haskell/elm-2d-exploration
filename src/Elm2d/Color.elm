module Elm2d.Color exposing
    ( Color
    , rgb, rgba, hex
    , white, black, transparent
    , red, green, blue
    , yellow, magenta, cyan
    , toCssString, toVector
    , toWebGlClearColor
    )

{-|

@docs Color
@docs rgb, rgba, hex

@docs white, black, transparent
@docs red, green, blue
@docs yellow, magenta, cyan

@docs toCssString, toVector
@docs toWebGlClearColor

-}

import Math.Vector4
import WebGL


type Color
    = Color Int Int Int Float


rgb : Int -> Int -> Int -> Color
rgb r g b =
    Color r g b 1


rgba : Int -> Int -> Int -> Float -> Color
rgba r g b a =
    Color r g b a


hex : String -> Color
hex hexString =
    fromHexToColor hexString


transparent : Color
transparent =
    Color 0 0 0 0


black : Color
black =
    Color 0 0 0 1


white : Color
white =
    Color 255 255 255 1


red : Color
red =
    Color 255 0 0 1


blue : Color
blue =
    Color 0 0 255 1


green : Color
green =
    Color 0 255 0 1


yellow : Color
yellow =
    Color 255 255 0 1


magenta : Color
magenta =
    Color 255 0 255 1


cyan : Color
cyan =
    Color 0 255 255 1


toCssString : Color -> String
toCssString (Color r g b a) =
    "rgba("
        ++ String.fromInt r
        ++ ","
        ++ String.fromInt g
        ++ ","
        ++ String.fromInt b
        ++ ","
        ++ String.fromFloat a
        ++ ")"


toVector : Color -> Math.Vector4.Vec4
toVector (Color r g b a) =
    Math.Vector4.vec4
        (toFloat r / 255)
        (toFloat g / 255)
        (toFloat b / 255)
        a


toWebGlClearColor : Color -> WebGL.Option
toWebGlClearColor (Color r g b a) =
    WebGL.clearColor (toFloat r / 255) (toFloat g / 255) (toFloat b / 255) a



-- HEX HELPER


fromHexToColor : String -> Color
fromHexToColor hexString =
    let
        maybeColor =
            case String.split "" hexString of
                "#" :: r :: g :: b :: [] ->
                    rgb
                        (fromHexToInt r)
                        (fromHexToInt g)
                        (fromHexToInt b)
                        |> Just

                "#" :: r :: g :: b :: a :: [] ->
                    rgba
                        (fromHexToInt r)
                        (fromHexToInt g)
                        (fromHexToInt b)
                        (fromHexToFloat a)
                        |> Just

                "#" :: r2 :: r1 :: g2 :: g1 :: b2 :: b1 :: [] ->
                    rgb
                        (fromHexToInt (r2 ++ r1))
                        (fromHexToInt (g2 ++ g1))
                        (fromHexToInt (b2 ++ b1))
                        |> Just

                "#" :: r2 :: r1 :: g2 :: g1 :: b2 :: b1 :: a2 :: a1 :: [] ->
                    rgba
                        (fromHexToInt (r2 ++ r1))
                        (fromHexToInt (g2 ++ g1))
                        (fromHexToInt (b2 ++ b1))
                        (fromHexToFloat (a2 ++ a1))
                        |> Just

                _ ->
                    Nothing
    in
    case maybeColor of
        Just color ->
            color

        Nothing ->
            Color 0 0 0 1


{-| Convert a 2-digit hex string into an Int from 0 to 255

    fromHexToInt "FF" == 255
    fromHexToInt "0A" == 10
    fromHexToInt "A0" == 160
    fromHexToInt "00" == 0

-}
fromHexToInt : String -> Int
fromHexToInt hexString =
    String.toList hexString
        |> List.filterMap fromHexCharToValue
        |> List.foldl (\x acc -> acc * 16 + x) 0


fromHexToFloat : String -> Float
fromHexToFloat hexString =
    fromHexToInt hexString
        |> toFloat
        |> (\x -> x / 255)


fromHexCharToValue : Char -> Maybe Int
fromHexCharToValue char =
    case Char.toUpper char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'A' ->
            Just 10

        'B' ->
            Just 11

        'C' ->
            Just 12

        'D' ->
            Just 13

        'E' ->
            Just 14

        'F' ->
            Just 15

        _ ->
            Nothing
