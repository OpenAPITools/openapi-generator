module DateOnly exposing (DateOnly, dateOnlyDecoder, dateOnlyEncoder)

import Date
import Date.Extra exposing (fromIsoString, toFormattedString)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias DateOnly =
    Date.Date


dateOnlyDecoder : Decoder DateOnly
dateOnlyDecoder =
    Decode.string
        |> Decode.andThen decodeIsoString


dateOnlyEncoder : DateOnly -> Encode.Value
dateOnlyEncoder model =
    Encode.string <| toFormattedString "yyyy-MM-dd" model


decodeIsoString : String -> Decoder DateOnly
decodeIsoString str =
    case fromIsoString str of
        Just date ->
            Decode.succeed date

        Nothing ->
            Decode.fail <|
                "Cannot convert "
                    ++ str
                    ++ " to DateOnly"
