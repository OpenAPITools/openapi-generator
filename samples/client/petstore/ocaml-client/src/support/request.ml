let base_url = "http://petstore.swagger.io/v2"
let default_headers = Cohttp.Header.init_with "Content-Type" "application/json"

let build_uri operation_path = Uri.of_string (base_url ^ operation_path)
let build_body to_json payload =
  to_json payload |> Yojson.Safe.to_string ~std:true |> Cohttp_lwt.Body.of_string
