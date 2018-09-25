module DateTime exposing (DateTime, decoder, encoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result
import Time


type alias DateTime =
    Time.Posix


decoder : Decoder DateTime
decoder =
    Decode.string
        |> Decode.andThen decodeIsoString


encoder : DateTime -> Encode.Value
encoder model =
    Encode.string <| Iso8601.fromTime model


decodeIsoString : String -> Decoder DateTime
decodeIsoString str =
    case Iso8601.toTime str of
        Result.Ok posix ->
            Decode.succeed posix

        Result.Err _ ->
            Decode.fail <| "Invalid date: " ++ str
