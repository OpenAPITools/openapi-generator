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

