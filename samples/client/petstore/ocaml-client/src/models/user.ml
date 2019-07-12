(**
    OCaml Rest Client
*)
(* user : A User who is purchasing from the pet store *)




type user = {
    id: Int64.t option;
    username: string option;
    first_name: string option;
    last_name: string option;
    email: string option;
    password: string option;
    phone: string option;
    (* User Status *)
    user_status: int option;
}

(** A User who is purchasing from the pet store *)
let create () : user = {
    id = None;
    username = None;
    first_name = None;
    last_name = None;
    email = None;
    password = None;
    phone = None;
    user_status = None;
}

