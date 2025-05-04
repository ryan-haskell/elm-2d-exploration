module Elm2d.Angle exposing
    ( Angle, zero
    , fromRadians, fromDegrees
    , add, lerp
    , toRadians, toDegrees
    )

{-|

@docs Angle, zero
@docs fromRadians, fromDegrees

@docs add, lerp

@docs toRadians, toDegrees

-}


type Angle
    = Angle Float


zero : Angle
zero =
    Angle 0


fromRadians : Float -> Angle
fromRadians radians =
    Angle radians


fromDegrees : Float -> Angle
fromDegrees degrees =
    Angle (degrees * pi / 180)


add : Angle -> Angle -> Angle
add (Angle a) (Angle b) =
    Angle
        (if (a + b) > 2 * pi then
            (a + b) - 2 * pi

         else if (a + b) < 0 then
            (a + b) + 2 * pi

         else
            a + b
        )


{-| Unlike `Float.lerp`, `Angle.lerp` will smoothly transition from 2pi to 0 without skipping 0.
-}
lerp : Angle -> Angle -> Float -> Angle
lerp (Angle a) (Angle b) t =
    Angle (lerpAngle a b t)


toRadians : Angle -> Float
toRadians (Angle radians) =
    radians


toDegrees : Angle -> Float
toDegrees (Angle radians) =
    radians * 180 / pi



-- Internal helper functions


floatMod : Float -> Float -> Float
floatMod x m =
    x - m * toFloat (floor (x / m))


lerpAngle : Float -> Float -> Float -> Float
lerpAngle a b t =
    let
        twoPi =
            2 * pi

        -- Normalize delta to range (-pi, pi]
        delta =
            ((b - a + pi)
                |> floatMod twoPi
            )
                - pi
    in
    a + delta * t
