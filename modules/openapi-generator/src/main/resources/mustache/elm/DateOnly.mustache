module DateOnly exposing (DateOnly, decoder, encode, toString)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result
import Time


type alias DateOnly =
    Time.Posix


decoder : Decoder DateOnly
decoder =
    Decode.string
        |> Decode.andThen decodeIsoString


encode : DateOnly -> Encode.Value
encode =
    Encode.string << toString


decodeIsoString : String -> Decoder DateOnly
decodeIsoString str =
    case Iso8601.toTime (str ++ "T00:00:00.000Z") of
        Result.Ok posix ->
            Decode.succeed posix

        Result.Err _ ->
            Decode.fail <| "Invalid date: " ++ str


toString : DateOnly -> String
toString =
    String.left 10 << Iso8601.fromTime
