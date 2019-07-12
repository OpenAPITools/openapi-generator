(**
    OCaml Rest Client
*)
(* api_response : Describes the result of uploading an image resource *)




type api_response = {
    code: int option;
    _type: string option;
    message: string option;
}

(** Describes the result of uploading an image resource *)
let create () : api_response = {
    code = None;
    _type = None;
    message = None;
}

