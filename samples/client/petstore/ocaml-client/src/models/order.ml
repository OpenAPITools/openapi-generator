(**
    OCaml Rest Client
*)
(* order : An order for a pets from the pet store *)



(** Order Status *)
type status =
| Placed
| Approved
| Delivered

type order = {
    id: Int64.t option;
    pet_id: Int64.t option;
    quantity: int option;
    ship_date: string option;
    (* Order Status *)
    status: [`Placed | `Approved | `Delivered] option;
    complete: bool option;
}

(** An order for a pets from the pet store *)
let create () : order = {
    id = None;
    pet_id = None;
    quantity = None;
    ship_date = None;
    status = None;
    complete = None;
}

