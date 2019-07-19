open Ppx_deriving_yojson_runtime

let to_int json =
    match json with
    | `Int x -> Result.Ok x
    | `Intlit s -> Result.Ok (int_of_string s)
    | _ -> Result.Error "JsonSupport.to_int"

let to_bool json =
    match json with
    | `Bool x -> Result.Ok x
    | _ -> Result.Error "JsonSupport.to_bool"

let to_float json =
    match json with
    | `Float x -> Result.Ok x
    | _ -> Result.Error "JsonSupport.to_float"

let to_string json =
    match json with
    | `String s -> Result.Ok s
    | _ -> Result.Error "JsonSupport.to_string"

let to_int32 json : int32 Ppx_deriving_yojson_runtime.error_or =
    match json with
    | `Int x -> Result.Ok (Int32.of_int x)
    | `Intlit s -> Result.Ok (Int32.of_string s)
    | _ -> Result.Error "JsonSupport.to_int32"

let to_int64 json : int64 Ppx_deriving_yojson_runtime.error_or =
    match json with
    | `Int x -> Result.Ok (Int64.of_int x)
    | `Intlit s -> Result.Ok (Int64.of_string s)
    | _ -> Result.Error "JsonSupport.to_int64"

let of_int x = `Int x

let of_bool b = `Bool b

let of_float x = `Float x

let of_string s = `String s

let of_int32 x = `Intlit (Int32.to_string x)

let of_int64 x = `Intlit (Int64.to_string x)

let of_list_of of_f l = `List (List.map of_f l)