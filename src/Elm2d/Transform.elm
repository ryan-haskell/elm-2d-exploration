module Elm2d.Transform exposing
    ( Transform, new
    , fromSize
    , setPosition, setRotation, setScale
    , move, moveX, moveY, rotate, scale
    , toPosition, toRotation, toScale
    , toMatrix
    )

{-|

@docs Transform, new
@docs fromSize

@docs setPosition, setRotation, setScale
@docs move, moveX, moveY, rotate, scale
@docs toPosition, toRotation, toScale

@docs toMatrix

-}

import Elm2d.Angle as Angle exposing (Angle)
import Elm2d.Vector as Vector exposing (Vector)
import Math.Matrix4
import Math.Vector3


type Transform
    = Transform
        { origin : Vector
        , sizeScale : Vector
        , position : Vector
        , rotation : Angle
        , scale : Vector
        }


new : Transform
new =
    Transform
        { origin = Vector.zero
        , position = Vector.zero
        , rotation = Angle.zero
        , scale = Vector.one
        , sizeScale = Vector.one
        }


fromSize : ( Int, Int ) -> Transform
fromSize intTuple =
    let
        vector =
            Vector.fromIntTuple intTuple
    in
    Transform
        { position = Vector.zero
        , rotation = Angle.zero
        , scale = Vector.one
        , sizeScale = vector
        , origin = vector |> Vector.scale 0.5
        }


toMatrix :
    Transform
    -> Math.Matrix4.Mat4
toMatrix (Transform data) =
    let
        ( originX, originY ) =
            Vector.toTuple data.origin
    in
    Math.Matrix4.identity
        |> Math.Matrix4.translate3
            originX
            originY
            0
        |> Math.Matrix4.rotate
            (Angle.toRadians data.rotation)
            (Math.Vector3.vec3 0 0 1)
        |> Math.Matrix4.translate3
            -originX
            -originY
            0
        |> Math.Matrix4.translate3
            (Vector.x data.position)
            (Vector.y data.position)
            0
        |> Math.Matrix4.scale3
            (Vector.x data.scale * Vector.x data.sizeScale)
            (Vector.y data.scale * Vector.y data.sizeScale)
            1



-- SETTING VALUES


setPosition : Vector -> Transform -> Transform
setPosition position (Transform transform) =
    Transform { transform | position = position }


setRotation : Angle -> Transform -> Transform
setRotation rotation (Transform transform) =
    Transform { transform | rotation = rotation }


setScale : Vector -> Transform -> Transform
setScale scale_ (Transform transform) =
    Transform { transform | scale = scale_ }



-- MODIFYING VALUES


move : Vector -> Transform -> Transform
move vector (Transform transform) =
    Transform { transform | position = Vector.add vector transform.position }


moveX : Float -> Transform -> Transform
moveX x (Transform transform) =
    Transform { transform | position = Vector.new x (Vector.y transform.position) }


moveY : Float -> Transform -> Transform
moveY y (Transform transform) =
    Transform { transform | position = Vector.new (Vector.x transform.position) y }


rotate : Angle -> Transform -> Transform
rotate angle (Transform transform) =
    Transform { transform | rotation = Angle.add angle transform.rotation }


scale : Vector -> Transform -> Transform
scale scale_ (Transform transform) =
    Transform { transform | scale = Vector.multiply scale_ transform.scale }



-- GETTING VALUES


toPosition : Transform -> Vector
toPosition (Transform transform) =
    transform.position


toRotation : Transform -> Angle
toRotation (Transform transform) =
    transform.rotation


toScale : Transform -> Vector
toScale (Transform transform) =
    transform.scale
