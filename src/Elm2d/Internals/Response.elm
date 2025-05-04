module Elm2d.Internals.Response exposing (Response(..), fromResult, toMaybe)


type Response error value
    = Loading
    | Success value
    | Failure error


fromResult : Result error value -> Response error value
fromResult result =
    case result of
        Ok value ->
            Success value

        Err error ->
            Failure error


toMaybe : Response x value -> Maybe value
toMaybe response =
    case response of
        Loading ->
            Nothing

        Success value ->
            Just value

        Failure _ ->
            Nothing
