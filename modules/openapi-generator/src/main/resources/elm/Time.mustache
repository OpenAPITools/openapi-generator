module Api.Time exposing
    ( Posix
    , dateToString
    , dateTimeToString
    , encodeDate
    , encodeDateTime
    , dateDecoder
    , dateTimeDecoder
    )

import Http
import Iso8601
import Json.Decode
import Json.Encode
import Time


type alias Posix = Time.Posix


dateToString : Posix -> String
dateToString =
    String.left 10 << dateTimeToString


dateTimeToString : Posix -> String
dateTimeToString =
    Iso8601.fromTime


encodeDate : Posix -> Json.Encode.Value
encodeDate =
    Json.Encode.string << dateToString


encodeDateTime : Posix -> Json.Encode.Value
encodeDateTime =
    Json.Encode.string << dateTimeToString


dateDecoder : Json.Decode.Decoder Posix
dateDecoder =
    Json.Decode.string
        |> Json.Decode.andThen decodeDateIsoString


decodeDateIsoString : String -> Json.Decode.Decoder Posix
decodeDateIsoString str =
    case Iso8601.toTime (str ++ "T00:00:00.000Z") of
        Result.Ok posix ->
            Json.Decode.succeed posix

        Result.Err _ ->
            Json.Decode.fail ("Invalid calendar date: " ++ str)


dateTimeDecoder : Json.Decode.Decoder Posix
dateTimeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen decodeDateTimeIsoString


decodeDateTimeIsoString : String -> Json.Decode.Decoder Posix
decodeDateTimeIsoString str =
    case Iso8601.toTime str of
        Result.Ok posix ->
            Json.Decode.succeed posix

        Result.Err _ ->
            Json.Decode.fail ("Invalid ISO date: " ++ str)