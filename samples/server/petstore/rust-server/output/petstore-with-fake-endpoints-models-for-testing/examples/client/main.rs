#![allow(missing_docs, unused_variables, trivial_casts)]


#[allow(unused_imports)]
use futures::{future, Stream, stream};
#[allow(unused_imports)]
use petstore_with_fake_endpoints_models_for_testing::{Api, ApiNoContext, Claims, Client, ContextWrapperExt, models,
                      TestSpecialTagsResponse,
                      Call123exampleResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
                      FakeResponseWithNumericalDescriptionResponse,
                      TestBodyWithQueryParamsResponse,
                      TestClientModelResponse,
                      TestEndpointParametersResponse,
                      TestEnumParametersResponse,
                      TestInlineAdditionalPropertiesResponse,
                      TestJsonFormDataResponse,
                      HyphenParamResponse,
                      TestClassnameResponse,
                      AddPetResponse,
                      FindPetsByStatusResponse,
                      FindPetsByTagsResponse,
                      UpdatePetResponse,
                      DeletePetResponse,
                      GetPetByIdResponse,
                      UpdatePetWithFormResponse,
                      UploadFileResponse,
                      GetInventoryResponse,
                      PlaceOrderResponse,
                      DeleteOrderResponse,
                      GetOrderByIdResponse,
                      CreateUserResponse,
                      CreateUsersWithArrayInputResponse,
                      CreateUsersWithListInputResponse,
                      LoginUserResponse,
                      LogoutUserResponse,
                      DeleteUserResponse,
                      GetUserByNameResponse,
                      UpdateUserResponse,
                     };
use clap::{Command, Arg};

// NOTE: Set environment variable RUST_LOG to the name of the executable (or "cargo run") to activate console logging for all loglevels.
//     See https://docs.rs/env_logger/latest/env_logger/  for more details

#[allow(unused_imports)]
use log::info;

// swagger::Has may be unused if there are no examples
#[allow(unused_imports)]
use swagger::{AuthData, ContextBuilder, EmptyContext, Has, Push, XSpanIdString};

type ClientContext = swagger::make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanIdString);

mod client_auth;
use client_auth::build_token;


// rt may be unused if there are no examples
#[allow(unused_mut)]
fn main() {
    env_logger::init();

    let matches = Command::new("client")
        .arg(Arg::new("operation")
            .help("Sets the operation to run")
            .value_parser([
                "TestSpecialTags",
                "Call123example",
                "FakeOuterBooleanSerialize",
                "FakeOuterCompositeSerialize",
                "FakeOuterNumberSerialize",
                "FakeOuterStringSerialize",
                "FakeResponseWithNumericalDescription",
                "TestBodyWithQueryParams",
                "TestClientModel",
                "TestEndpointParameters",
                "TestEnumParameters",
                "TestInlineAdditionalProperties",
                "TestJsonFormData",
                "HyphenParam",
                "TestClassname",
                "AddPet",
                "FindPetsByStatus",
                "FindPetsByTags",
                "UpdatePet",
                "DeletePet",
                "GetPetById",
                "UpdatePetWithForm",
                "UploadFile",
                "GetInventory",
                "PlaceOrder",
                "DeleteOrder",
                "GetOrderById",
                "CreateUser",
                "CreateUsersWithArrayInput",
                "CreateUsersWithListInput",
                "LoginUser",
                "LogoutUser",
                "DeleteUser",
                "GetUserByName",
                "UpdateUser",
            ])
            .required(true)
            .index(1))
        .arg(Arg::new("https")
            .long("https")
            .help("Whether to use HTTPS or not"))
        .arg(Arg::new("host")
            .long("host")
            .default_value("petstore.swagger.io")
            .help("Hostname to contact"))
        .arg(Arg::new("port")
            .long("port")
            .default_value("80")
            .help("Port to contact"))
        .get_matches();

    // Create Bearer-token with a fixed key (secret) for test purposes.
    // In a real (production) system this Bearer token should be obtained via an external Identity/Authentication-server
    // Ensure that you set the correct algorithm and encodingkey that matches what is used on the server side.
    // See https://github.com/Keats/jsonwebtoken for more information
    let auth_token = build_token(
            Claims {
                sub: "tester@acme.com".to_owned(),
                company: "ACME".to_owned(),
                iss: "my_identity_provider".to_owned(),
                // added a very long expiry time
                aud: "org.acme.Resource_Server".to_string(),
                exp: 10000000000,
                // In this example code all available Scopes are added, so the current Bearer Token gets fully authorization.
                scopes:
                  [
                            "write:pets",
                            "read:pets",
                  ].join::<&str>(", ")
            },
            b"secret").unwrap();

    let auth_data = if !auth_token.is_empty() {
        Some(AuthData::Bearer(auth_token))
    } else {
        // No Bearer-token available, so return None
        None
    };

    let is_https = matches.contains_id("https");
    let base_url = format!("{}://{}:{}",
        if is_https { "https" } else { "http" },
        matches.get_one::<String>("host").unwrap(),
        matches.get_one::<u16>("port").unwrap());

    let context: ClientContext =
        swagger::make_context!(ContextBuilder, EmptyContext, auth_data, XSpanIdString::default());

    let mut client : Box<dyn ApiNoContext<ClientContext>> = if is_https {
        // Using Simple HTTPS
        let client = Box::new(Client::try_new_https(&base_url)
            .expect("Failed to create HTTPS client"));
        Box::new(client.with_context(context))
    } else {
        // Using HTTP
        let client = Box::new(Client::try_new_http(
            &base_url)
            .expect("Failed to create HTTP client"));
        Box::new(client.with_context(context))
    };

    let mut rt = tokio::runtime::Runtime::new().unwrap();

    match matches.get_one::<String>("operation").map(String::as_str) {
        /* Disabled because there's no example.
        Some("TestSpecialTags") => {
            let result = rt.block_on(client.test_special_tags(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("Call123example") => {
            let result = rt.block_on(client.call123example(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("FakeOuterBooleanSerialize") => {
            let result = rt.block_on(client.fake_outer_boolean_serialize(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("FakeOuterCompositeSerialize") => {
            let result = rt.block_on(client.fake_outer_composite_serialize(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("FakeOuterNumberSerialize") => {
            let result = rt.block_on(client.fake_outer_number_serialize(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("FakeOuterStringSerialize") => {
            let result = rt.block_on(client.fake_outer_string_serialize(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("FakeResponseWithNumericalDescription") => {
            let result = rt.block_on(client.fake_response_with_numerical_description(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("TestBodyWithQueryParams") => {
            let result = rt.block_on(client.test_body_with_query_params(
                  "query_example".to_string(),
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        /* Disabled because there's no example.
        Some("TestClientModel") => {
            let result = rt.block_on(client.test_client_model(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("TestEndpointParameters") => {
            let result = rt.block_on(client.test_endpoint_parameters(
                  8.14,
                  1.2,
                  "pattern_without_delimiter_example".to_string(),
                  swagger::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE")),
                  Some(56),
                  Some(56),
                  Some(789),
                  Some(3.4),
                  Some("string_example".to_string()),
                  Some(swagger::ByteArray(Vec::from("BINARY_DATA_HERE"))),
                  None,
                  None,
                  Some("password_example".to_string()),
                  Some("callback_example".to_string())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("TestEnumParameters") => {
            let result = rt.block_on(client.test_enum_parameters(
                  Some(&Vec::new()),
                  None,
                  Some(&Vec::new()),
                  None,
                  None,
                  None,
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("TestInlineAdditionalProperties") => {
            let result = rt.block_on(client.test_inline_additional_properties(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("TestJsonFormData") => {
            let result = rt.block_on(client.test_json_form_data(
                  "param_example".to_string(),
                  "param2_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("HyphenParam") => {
            let result = rt.block_on(client.hyphen_param(
                  "hyphen_param_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("TestClassname") => {
            let result = rt.block_on(client.test_classname(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        /* Disabled because there's no example.
        Some("AddPet") => {
            let result = rt.block_on(client.add_pet(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("FindPetsByStatus") => {
            let result = rt.block_on(client.find_pets_by_status(
                  &Vec::new()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("FindPetsByTags") => {
            let result = rt.block_on(client.find_pets_by_tags(
                  &Vec::new()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("UpdatePet") => {
            let result = rt.block_on(client.update_pet(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("DeletePet") => {
            let result = rt.block_on(client.delete_pet(
                  789,
                  Some("api_key_example".to_string())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("GetPetById") => {
            let result = rt.block_on(client.get_pet_by_id(
                  789
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("UpdatePetWithForm") => {
            let result = rt.block_on(client.update_pet_with_form(
                  789,
                  Some("name_example".to_string()),
                  Some("status_example".to_string())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("UploadFile") => {
            let result = rt.block_on(client.upload_file(
                  789,
                  Some("additional_metadata_example".to_string()),
                  Some(swagger::ByteArray(Vec::from("BINARY_DATA_HERE")))
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("GetInventory") => {
            let result = rt.block_on(client.get_inventory(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("PlaceOrder") => {
            let result = rt.block_on(client.place_order(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("DeleteOrder") => {
            let result = rt.block_on(client.delete_order(
                  "order_id_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("GetOrderById") => {
            let result = rt.block_on(client.get_order_by_id(
                  789
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("CreateUser") => {
            let result = rt.block_on(client.create_user(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("CreateUsersWithArrayInput") => {
            let result = rt.block_on(client.create_users_with_array_input(
                  &Vec::new()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("CreateUsersWithListInput") => {
            let result = rt.block_on(client.create_users_with_list_input(
                  &Vec::new()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("LoginUser") => {
            let result = rt.block_on(client.login_user(
                  "username_example".to_string(),
                  "password_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("LogoutUser") => {
            let result = rt.block_on(client.logout_user(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("DeleteUser") => {
            let result = rt.block_on(client.delete_user(
                  "username_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("GetUserByName") => {
            let result = rt.block_on(client.get_user_by_name(
                  "username_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("UpdateUser") => {
            let result = rt.block_on(client.update_user(
                  "username_example".to_string(),
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        _ => {
            panic!("Invalid operation provided")
        }
    }
}
