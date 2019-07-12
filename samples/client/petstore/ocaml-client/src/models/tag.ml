(**
    OCaml Rest Client
*)
(* tag : A tag for a pet *)




type tag = {
    id: Int64.t option;
    name: string option;
}

(** A tag for a pet *)
let create () : tag = {
    id = None;
    name = None;
}

