module DateTime exposing (DateTime, dateTimeDecoder, dateTimeEncoder)

import Date
import Date.Extra exposing (fromIsoString, toIsoString)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result


type alias DateTime =
    Date.Date


dateTimeDecoder : Decoder DateTime
dateTimeDecoder =
    Decode.string
        |> Decode.andThen decodeIsoString


dateTimeEncoder : DateTime -> Encode.Value
dateTimeEncoder model =
    Encode.string <| toIsoString model


decodeIsoString : String -> Decoder DateTime
decodeIsoString str =
    case fromIsoString str of
        Result.Ok date ->
            Decode.succeed date

        Result.Err msg ->
            Decode.fail msg
