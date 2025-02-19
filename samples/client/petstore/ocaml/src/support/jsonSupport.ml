open Ppx_deriving_yojson_runtime

let unwrap to_json json =
    match to_json json with
    | Result.Ok json -> json
    | Result.Error s -> failwith s

let to_int json =
    match json with
    | `Int x -> x
    | `Intlit s -> int_of_string s
    | _ -> failwith "JsonSupport.to_int"

let to_bool json =
    match json with
    | `Bool x -> x
    | _ -> failwith "JsonSupport.to_bool"

let to_float json =
    match json with
    | `Float x -> x
    | _ -> failwith "JsonSupport.to_float"

let to_string json =
    match json with
    | `String s -> s
    | _ -> failwith "JsonSupport.to_string"

let to_int32 json : int32 =
    match json with
    | `Int x -> Int32.of_int x
    | `Intlit s -> Int32.of_string s
    | _ -> failwith "JsonSupport.to_int32"

let to_int64 json : int64 =
    match json with
    | `Int x -> Int64.of_int x
    | `Intlit s -> Int64.of_string s
    | _ -> failwith "JsonSupport.to_int64"

let of_int x = `Int x

let of_bool b = `Bool b

let of_float x = `Float x

let of_string s = `String s

let of_int32 x = `Intlit (Int32.to_string x)

let of_int64 x = `Intlit (Int64.to_string x)

let of_list_of of_f l = `List (List.map of_f l)

let of_map_of of_f l = `Assoc (List.map (fun (k, v) -> (k, of_f v)) l)