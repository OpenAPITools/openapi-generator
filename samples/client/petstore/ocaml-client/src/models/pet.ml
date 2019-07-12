(**
    OCaml Rest Client
*)
(* pet : A pet for sale in the pet store *)



(** pet status in the store *)
type status =
| Available
| Pending
| Sold

type pet = {
    id: Int64.t option;
    category: category option;
    name: string;
    photo_urls: string list;
    tags: tag list;
    (* pet status in the store *)
    status: [`Available | `Pending | `Sold] option;
}

(** A pet for sale in the pet store *)
let create (name : string) (photo_urls : string list) : pet = {
    id = None;
    category = None;
    name = name;
    photo_urls = photo_urls;
    tags = [];
    status = None;
}

