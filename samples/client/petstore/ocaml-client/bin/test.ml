open Petstore_client
open Lwt

let lift f r = Ppx_deriving_yojson_runtime.(>|=) r f

let main () =
  let p = Pet.create "Bob" [] in
  Pet_api.add_pet p >|= Yojson.Safe.to_string >|= print_endline

let find_pet () =
  let pet_id = 9199424981609334637L in
  Pet_api.get_pet_by_id pet_id >|= Pet.show >|= print_endline

let find_pets_by_tags () =
  let tags = ["dog"; "cat"] in
  Pet_api.find_pets_by_tags tags >|= List.map Pet.show >|= List.map print_endline

let _ =
  Lwt_main.run (find_pets_by_tags ())
