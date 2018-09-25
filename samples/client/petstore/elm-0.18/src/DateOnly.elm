module DateOnly exposing (DateOnly, decoder, encoder)

import Date
import Date.Extra exposing (fromIsoString, toFormattedString)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result


type alias DateOnly =
    Date.Date


decoder : Decoder DateOnly
decoder =
    Decode.string
        |> Decode.andThen decodeIsoString


encoder : DateOnly -> Encode.Value
encoder model =
    Encode.string <| toFormattedString "yyyy-MM-dd" model


decodeIsoString : String -> Decoder DateOnly
decodeIsoString str =
    case fromIsoString str of
        Result.Ok date ->
            Decode.succeed date

        Result.Err msg ->
            Decode.fail msg
