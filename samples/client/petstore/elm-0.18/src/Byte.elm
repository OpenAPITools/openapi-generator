module Byte exposing (Byte, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Byte =
    String


decoder : Decoder Byte
decoder =
    Decode.string


encode : Byte -> Encode.Value
encode model =
    Encode.string model
