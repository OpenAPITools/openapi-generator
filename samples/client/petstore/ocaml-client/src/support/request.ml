let api_key = ""
let base_url = "http://petstore.swagger.io/v2"
let default_headers = Cohttp.Header.init_with "Content-Type" "application/json"

let build_uri operation_path = Uri.of_string (base_url ^ operation_path)
let write_json_body to_json payload =
  to_json payload |> Yojson.Safe.to_string ~std:true |> Cohttp_lwt.Body.of_string

let handle_response resp on_success_handler =
  match Cohttp_lwt.Response.status resp with
  | #Cohttp.Code.success_status -> on_success_handler ()
  | s -> failwith ("Server responded with status " ^ Cohttp.Code.(reason_phrase_of_code (code_of_status s)))

let handle_unit_response resp = handle_response resp (fun () -> Lwt.return ())	
	
let read_json_body resp body =
  handle_response resp (fun () ->
    (Lwt.(Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string)))

let read_json_body_as of_json resp body =
  Lwt.(read_json_body resp body >|= of_json)

let read_json_body_as_list resp body =
  Lwt.(read_json_body resp body >|= Yojson.Safe.Util.to_list)

let read_json_body_as_list_of of_json resp body =
  Lwt.(read_json_body_as_list resp body >|= List.map of_json)

let read_json_body_as_map_of of_json resp body =
  Lwt.(read_json_body resp body >|= Yojson.Safe.Util.to_assoc >|= List.map (fun (s, v) -> (s, of_json v)))

let replace_path_param uri param_name param_value =
  let regexp = Str.regexp (Str.quote ("{" ^ param_name ^ "}")) in
  let path = Str.global_replace regexp param_value (Uri.path uri) in
  Uri.with_path uri path

let init_form_encoded_body () = ""

let add_form_encoded_body_param params (paramName, paramValue) =
  let new_param_enc = Printf.sprintf {|%s=%s|} (Uri.pct_encode paramName) (Uri.pct_encode paramValue) in
  if params = ""
  then new_param_enc
  else Printf.sprintf {|%s&%s|} params new_param_enc

let add_form_encoded_body_params params (paramName, new_params) =
  add_form_encoded_body_param params (paramName, String.concat "," new_params)

let finalize_form_encoded_body body = Cohttp_lwt.Body.of_string body
