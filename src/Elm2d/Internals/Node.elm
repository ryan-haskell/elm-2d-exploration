module Elm2d.Internals.Node exposing (..)

import Elm2d.Camera as Camera exposing (Camera)
import Elm2d.Color as Color exposing (Color)
import Elm2d.Internals.Node.Image as Image
import Elm2d.Internals.Node.Rectangle as Rectangle
import Elm2d.Internals.SpriteSheet as SpriteSheet exposing (SpriteSheet)
import Elm2d.Transform as Transform exposing (Transform)
import Elm2d.Vector as Vector exposing (Vector)
import WebGL
import WebGL.Texture as Texture exposing (Texture)


type Node msg
    = Node Internals


type alias Internals =
    { visual : Visual
    , transform : Transform
    }


type Visual
    = Visual_Rectangle Rectangle
    | Visual_Image Image
    | Visual_Custom WebGL.Entity


type alias Image =
    { url : String
    , size : ( Int, Int )
    , selection : Selection
    , flipX : Bool
    , flipY : Bool
    }


type Selection
    = FullSelection
    | FixedSelection ( Int, Int )
    | SpriteSelection String Int SpriteSheet


type alias Rectangle =
    { fill : Color
    , size : ( Int, Int )
    }



-- TEXTURE URLS


findTextureUrls : Node msg -> List String
findTextureUrls (Node node_) =
    case node_.visual of
        Visual_Rectangle _ ->
            []

        Visual_Image image ->
            [ image.url ]

        Visual_Custom _ ->
            []



-- RENDERING NODES


toWebGLEntity :
    { model
        | camera : Camera
        , getTexture : String -> Maybe Texture
    }
    -> Node msg
    -> WebGL.Entity
toWebGLEntity model (Node node) =
    case node.visual of
        Visual_Image image ->
            case ( model.getTexture image.url, image.selection ) of
                ( Just texture, FullSelection ) ->
                    Image.toWebGLEntity
                        { texture = texture
                        , size = image.size
                        , offset = ( 0, 0 )
                        , flipX = image.flipX
                        , flipY = image.flipY
                        , camera = Camera.toMatrix model.camera
                        , transform = Transform.toMatrix node.transform
                        }

                ( Just texture, FixedSelection offset ) ->
                    Image.toWebGLEntity
                        { texture = texture
                        , size = image.size
                        , offset = offset
                        , flipX = image.flipX
                        , flipY = image.flipY
                        , camera = Camera.toMatrix model.camera
                        , transform = Transform.toMatrix node.transform
                        }

                ( Just texture, SpriteSelection animation frame spriteSheet ) ->
                    Image.toWebGLEntity
                        { texture = texture
                        , size = image.size
                        , offset = SpriteSheet.getSelection animation frame texture spriteSheet
                        , camera = Camera.toMatrix model.camera
                        , transform = Transform.toMatrix node.transform
                        , flipX = image.flipX
                        , flipY = image.flipY
                        }

                _ ->
                    Rectangle.toWebGLEntity
                        { size = ( 1, 1 )
                        , fill = Color.magenta
                        , camera = Camera.toMatrix model.camera
                        , transform = Transform.toMatrix node.transform
                        }

        Visual_Rectangle rectangle ->
            Rectangle.toWebGLEntity
                { size = rectangle.size
                , fill = rectangle.fill
                , camera = Camera.toMatrix model.camera
                , transform = Transform.toMatrix node.transform
                }

        Visual_Custom entity ->
            entity
