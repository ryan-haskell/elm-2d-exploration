module Elm2d.Node exposing
    ( Node
    , rectangle, sprite, image, custom
    , setPosition, setRotation, setScale
    , move, rotate, scale, flipX, flipY
    , toPosition, toRotation, toScale
    , toTransform
    )

{-|

@docs Node
@docs rectangle, sprite, image, custom

@docs setPosition, setRotation, setScale
@docs move, rotate, scale, flipX, flipY
@docs toPosition, toRotation, toScale
@docs toTransform

-}

import Elm2d.Angle as Angle exposing (Angle)
import Elm2d.Color as Color exposing (Color)
import Elm2d.Internals.Node as Node exposing (..)
import Elm2d.Internals.SpriteSheet as SpriteSheet exposing (SpriteSheet)
import Elm2d.Transform as Transform exposing (Transform)
import Elm2d.Vector as Vector exposing (Vector)
import WebGL


type alias Node msg =
    Node.Node msg


image :
    { url : String
    , size : ( Int, Int )
    , offset : ( Int, Int )
    }
    -> Node msg
image info =
    Node
        { visual =
            Visual_Image
                { url = info.url
                , size = info.size
                , selection = FixedSelection info.offset
                , flipX = False
                , flipY = False
                }
        , transform = Transform.fromSize info.size
        }


sprite :
    { spriteSheet : SpriteSheet
    , animation : String
    , frame : Int
    }
    -> Node msg
sprite info =
    Node
        { visual =
            Visual_Image
                { url = SpriteSheet.toTextureUrl info.spriteSheet
                , size = SpriteSheet.toSize info.spriteSheet
                , selection = SpriteSelection info.animation info.frame info.spriteSheet
                , flipX = False
                , flipY = False
                }
        , transform = Transform.fromSize (SpriteSheet.toSize info.spriteSheet)
        }


custom : WebGL.Entity -> Node msg
custom entity =
    Node
        { visual = Visual_Custom entity
        , transform = Transform.new
        }


rectangle : { size : ( Int, Int ), fill : Color } -> Node msg
rectangle info =
    Node
        { visual = Visual_Rectangle info
        , transform = Transform.fromSize info.size
        }


setPosition : Vector -> Node msg -> Node msg
setPosition position (Node node) =
    Node { node | transform = Transform.setPosition position node.transform }


setRotation : Angle -> Node msg -> Node msg
setRotation rotation (Node node) =
    Node { node | transform = Transform.setRotation rotation node.transform }


setScale : Vector -> Node msg -> Node msg
setScale scale_ (Node node) =
    Node { node | transform = Transform.setScale scale_ node.transform }


move : Vector -> Node msg -> Node msg
move position (Node node) =
    Node { node | transform = Transform.move position node.transform }


rotate : Angle -> Node msg -> Node msg
rotate rotation (Node node) =
    Node { node | transform = Transform.rotate rotation node.transform }


scale : Vector -> Node msg -> Node msg
scale scale_ (Node node) =
    Node { node | transform = Transform.scale scale_ node.transform }


flipX : Node msg -> Node msg
flipX (Node node) =
    case node.visual of
        Visual_Image image_ ->
            Node { node | visual = Visual_Image { image_ | flipX = not image_.flipX } }

        _ ->
            Node node


flipY : Node msg -> Node msg
flipY (Node node) =
    case node.visual of
        Visual_Image image_ ->
            Node { node | visual = Visual_Image { image_ | flipY = not image_.flipY } }

        _ ->
            Node node


toPosition : Node msg -> Vector
toPosition (Node node) =
    Transform.toPosition node.transform


toRotation : Node msg -> Angle
toRotation (Node node) =
    Transform.toRotation node.transform


toScale : Node msg -> Vector
toScale (Node node) =
    Transform.toScale node.transform


toTransform : Node msg -> Transform
toTransform (Node node) =
    node.transform
