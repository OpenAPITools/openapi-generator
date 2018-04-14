module DateTime exposing (DateTime, dateTimeDecoder, dateTimeEncoder)

import Date
import Date.Extra exposing (fromIsoString, toIsoString)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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
        Just date ->
            Decode.succeed date

        Nothing ->
            Decode.fail <|
                "Cannot convert "
                    ++ str
                    ++ " to DateTime"
