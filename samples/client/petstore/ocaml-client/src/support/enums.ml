

(**  *)
type status =
| Placed [@printer fun fmt _ -> Format.pp_print_string fmt "placed"] [@name "placed"]
| Approved [@printer fun fmt _ -> Format.pp_print_string fmt "approved"] [@name "approved"]
| Delivered [@printer fun fmt _ -> Format.pp_print_string fmt "delivered"] [@name "delivered"]
[@@deriving yojson, show { with_path = false }];;


(**  *)
type pet_status =
| Available [@printer fun fmt _ -> Format.pp_print_string fmt "available"] [@name "available"]
| Pending [@printer fun fmt _ -> Format.pp_print_string fmt "pending"] [@name "pending"]
| Sold [@printer fun fmt _ -> Format.pp_print_string fmt "sold"] [@name "sold"]
[@@deriving yojson, show { with_path = false }];;
