module Elm2d.Internals.SpriteSheet exposing (..)

import Dict exposing (Dict)
import List.Extra
import WebGL.Texture as Texture exposing (Texture)


type SpriteSheet
    = SpriteSheet Internals


type alias Internals =
    { url : String
    , size : ( Int, Int )
    , gap : ( Int, Int )
    , animations : Dict String (List Int)
    }


toTextureUrl : SpriteSheet -> String
toTextureUrl (SpriteSheet internals) =
    internals.url


toSize : SpriteSheet -> ( Int, Int )
toSize (SpriteSheet internals) =
    internals.size


toGap : SpriteSheet -> ( Int, Int )
toGap (SpriteSheet internals) =
    internals.gap


getIndex : String -> Int -> SpriteSheet -> Maybe Int
getIndex animation frame (SpriteSheet internals) =
    case Dict.get animation internals.animations of
        Just frames ->
            List.Extra.getAt (modBy (List.length frames) frame) frames

        Nothing ->
            Nothing


getSelection : String -> Int -> Texture -> SpriteSheet -> ( Int, Int )
getSelection animation frame texture (SpriteSheet internals) =
    case
        ( getIndex animation frame (SpriteSheet internals)
        , internals.size
        , internals.gap
        )
    of
        ( Just index, ( width, height ), ( gapX, gapY ) ) ->
            if (width + gapX) == 0 then
                ( 0, 0 )

            else
                let
                    ( textureWidth, textureHeight ) =
                        Texture.size texture

                    tilesAcross =
                        textureWidth // (width + gapX)

                    ( x, y ) =
                        ( modBy tilesAcross index
                        , index // tilesAcross
                        )
                in
                ( x * (width + gapX)
                , y * (height + gapY)
                )

        _ ->
            ( 0, 0 )
