module Elm2d.SpriteSheet exposing
    ( SpriteSheet, new
    , withAnimation
    , withGap
    , toSize
    )

{-|

@docs SpriteSheet, new
@docs withAnimation
@docs withGap
@docs toSize

-}

import Dict
import Elm2d.Internals.SpriteSheet exposing (..)


type alias SpriteSheet =
    Elm2d.Internals.SpriteSheet.SpriteSheet


new :
    { url : String
    , size : ( Int, Int )
    }
    -> SpriteSheet
new { url, size } =
    SpriteSheet
        { url = url
        , size = size
        , gap = ( 0, 0 )
        , animations = Dict.empty
        }


withAnimation : String -> List Int -> SpriteSheet -> SpriteSheet
withAnimation name frames (SpriteSheet internals) =
    SpriteSheet { internals | animations = Dict.insert name frames internals.animations }


withGap : ( Int, Int ) -> SpriteSheet -> SpriteSheet
withGap gap (SpriteSheet internals) =
    SpriteSheet { internals | gap = gap }


toSize : SpriteSheet -> ( Float, Float )
toSize (SpriteSheet internals) =
    internals.size
        |> Tuple.mapBoth toFloat toFloat
