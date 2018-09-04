module DateOnly exposing (DateOnly, dateOnlyDecoder, dateOnlyEncoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result
import Time


type alias DateOnly =
    Time.Posix


dateOnlyDecoder : Decoder DateOnly
dateOnlyDecoder =
    Decode.string
        |> Decode.andThen decodeIsoString


dateOnlyEncoder : DateOnly -> Encode.Value
dateOnlyEncoder model =
    Iso8601.fromTime model
        |> String.left 10
        |> Encode.string


decodeIsoString : String -> Decoder DateOnly
decodeIsoString str =
    case Iso8601.toTime (str ++ "T00:00:00.000Z") of
        Result.Ok posix ->
            Decode.succeed posix

        Result.Err _ ->
            Decode.fail <| "Invalid date: " ++ str
