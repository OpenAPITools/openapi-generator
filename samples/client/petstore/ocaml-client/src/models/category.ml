(**
    OCaml Rest Client
*)
(* category : A category for a pet *)




type category = {
    id: Int64.t option;
    name: string option;
}

(** A category for a pet *)
let create () : category = {
    id = None;
    name = None;
}

