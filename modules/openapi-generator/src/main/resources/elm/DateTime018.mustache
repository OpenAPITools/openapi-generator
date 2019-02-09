module DateTime exposing (DateTime, decoder, encode, toString)

import Date
import Date.Extra exposing (fromIsoString, toIsoString)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result


type alias DateTime =
    Date.Date


decoder : Decoder DateTime
decoder =
    Decode.string
        |> Decode.andThen decodeIsoString


encode : DateTime -> Encode.Value
encode =
    Encode.string << toString


decodeIsoString : String -> Decoder DateTime
decodeIsoString str =
    case fromIsoString str of
        Result.Ok date ->
            Decode.succeed date

        Result.Err msg ->
            Decode.fail msg


toString : DateTime -> String
toString =
    toIsoString