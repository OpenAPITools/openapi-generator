let main () =
  let open Petstore_client in
  let open Lwt in
  let p = Pet.create "Bob" [] in
  Pet_api.add_pet p >>= fun (_r, b) ->
    b |> Cohttp_lwt.Body.to_string >|= print_endline
    
let _ =
  print_endline "coucou";
  Lwt_main.run (main ())
