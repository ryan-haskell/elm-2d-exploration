module Elm2d.Vector exposing
    ( Vector
    , new
    , zero, one
    , fromTuple, toTuple
    , fromIntTuple
    , fromRecord, toRecord
    , x, y
    , withX, withY
    , add, multiply
    , normalize
    , scale
    , dot, distance, direction
    , length, negate, clamp
    )

{-|

@docs Vector

@docs new
@docs zero, one
@docs fromTuple, toTuple
@docs fromIntTuple
@docs fromRecord, toRecord

@docs x, y
@docs withX, withY

@docs add, multiply
@docs normalize
@docs scale
@docs dot, distance, direction
@docs length, negate, clamp

-}

import Math.Vector2


type alias Vector =
    Math.Vector2.Vec2


zero : Vector
zero =
    Math.Vector2.vec2 0 0


one : Vector
one =
    Math.Vector2.vec2 1 1


new : Float -> Float -> Vector
new vx vy =
    Math.Vector2.vec2 vx vy


x : Vector -> Float
x vector =
    Math.Vector2.getX vector


y : Vector -> Float
y vector =
    Math.Vector2.getY vector


withX : Float -> Vector -> Vector
withX vx vector =
    Math.Vector2.setX vx vector


withY : Float -> Vector -> Vector
withY vy vector =
    Math.Vector2.setY vy vector


fromTuple : ( Float, Float ) -> Vector
fromTuple ( vx, vy ) =
    Math.Vector2.vec2 vx vy


fromIntTuple : ( Int, Int ) -> Vector
fromIntTuple ( vx, vy ) =
    Math.Vector2.vec2 (toFloat vx) (toFloat vy)


toTuple : Vector -> ( Float, Float )
toTuple vector =
    ( x vector, y vector )


fromRecord : { x : Float, y : Float } -> Vector
fromRecord record =
    Math.Vector2.fromRecord record


toRecord : Vector -> { x : Float, y : Float }
toRecord vector =
    Math.Vector2.toRecord vector


normalize : Vector -> Vector
normalize vector =
    Math.Vector2.normalize vector


clamp : Vector -> Vector -> Vector -> Vector
clamp minVec maxVec vector =
    let
        ( minX, minY ) =
            toTuple minVec

        ( maxX, maxY ) =
            toTuple maxVec

        ( vx, vy ) =
            toTuple vector
    in
    fromTuple
        ( Basics.max minX (Basics.min maxX vx)
        , Basics.max minY (Basics.min maxY vy)
        )


scale : Float -> Vector -> Vector
scale scalar vector =
    Math.Vector2.scale scalar vector


add : Vector -> Vector -> Vector
add vector1 vector2 =
    Math.Vector2.add vector1 vector2


multiply : Vector -> Vector -> Vector
multiply vector1 vector2 =
    Math.Vector2.vec2
        (x vector1 * x vector2)
        (y vector1 * y vector2)


dot : Vector -> Vector -> Float
dot vector1 vector2 =
    Math.Vector2.dot vector1 vector2


distance : Vector -> Vector -> Float
distance vector1 vector2 =
    Math.Vector2.distance vector1 vector2


direction : Vector -> Vector -> Vector
direction vector1 vector2 =
    Math.Vector2.direction vector1 vector2


length : Vector -> Float
length vector =
    Math.Vector2.length vector


negate : Vector -> Vector
negate vector =
    Math.Vector2.negate vector
