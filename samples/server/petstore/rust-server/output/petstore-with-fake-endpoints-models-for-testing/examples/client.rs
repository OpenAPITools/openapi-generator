#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate petstore_with_fake_endpoints_models_for_testing;
extern crate futures;
#[macro_use]
extern crate swagger;
extern crate clap;
extern crate tokio;

use swagger::{ContextBuilder, EmptyContext, XSpanIdString, Has, Push, AuthData};

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use petstore_with_fake_endpoints_models_for_testing::{Api, ApiNoContext, Client, ContextWrapperExt,
                      ApiError,
                      TestSpecialTagsResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
                      TestBodyWithQueryParamsResponse,
                      TestClientModelResponse,
                      TestEndpointParametersResponse,
                      TestEnumParametersResponse,
                      TestInlineAdditionalPropertiesResponse,
                      TestJsonFormDataResponse,
                      TestClassnameResponse,
                      AddPetResponse,
                      DeletePetResponse,
                      FindPetsByStatusResponse,
                      FindPetsByTagsResponse,
                      GetPetByIdResponse,
                      UpdatePetResponse,
                      UpdatePetWithFormResponse,
                      UploadFileResponse,
                      DeleteOrderResponse,
                      GetInventoryResponse,
                      GetOrderByIdResponse,
                      PlaceOrderResponse,
                      CreateUserResponse,
                      CreateUsersWithArrayInputResponse,
                      CreateUsersWithListInputResponse,
                      DeleteUserResponse,
                      GetUserByNameResponse,
                      LoginUserResponse,
                      LogoutUserResponse,
                      UpdateUserResponse
                     };
use clap::{App, Arg};

fn main() {
    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[

                "FakeOuterBooleanSerialize",

                "FakeOuterCompositeSerialize",

                "FakeOuterNumberSerialize",

                "FakeOuterStringSerialize",

                "TestEndpointParameters",

                "TestEnumParameters",

                "TestJsonFormData",

                "DeletePet",

                "FindPetsByStatus",

                "FindPetsByTags",

                "GetPetById",

                "UpdatePetWithForm",

                "UploadFile",

                "DeleteOrder",

                "GetInventory",

                "GetOrderById",

                "CreateUsersWithArrayInput",

                "CreateUsersWithListInput",

                "DeleteUser",

                "GetUserByName",

                "LoginUser",

                "LogoutUser",

            ])
            .required(true)
            .index(1))
        .arg(Arg::with_name("https")
            .long("https")
            .help("Whether to use HTTPS or not"))
        .arg(Arg::with_name("host")
            .long("host")
            .takes_value(true)
            .default_value("petstore.swagger.io")
            .help("Hostname to contact"))
        .arg(Arg::with_name("port")
            .long("port")
            .takes_value(true)
            .default_value("80")
            .help("Port to contact"))
        .get_matches();

    let is_https = matches.is_present("https");
    let base_url = format!("{}://{}:{}",
                           if is_https { "https" } else { "http" },
                           matches.value_of("host").unwrap(),
                           matches.value_of("port").unwrap());

    let client = if matches.is_present("https") {
        // Using Simple HTTPS
        Client::try_new_https(
            &base_url,
            "examples/ca.pem")
            .expect("Failed to create HTTPS client")
    } else {
        // Using HTTP
        Client::try_new_http(
            &base_url)
            .expect("Failed to create HTTP client")
    };

    let context: make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanIdString) =
        make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanIdString::default());

    let client = client.with_context(context);

    match matches.value_of("operation") {

        /* Disabled because there's no example.
        Some("TestSpecialTags") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_special_tags(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        Some("FakeOuterBooleanSerialize") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.fake_outer_boolean_serialize(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("FakeOuterCompositeSerialize") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.fake_outer_composite_serialize(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("FakeOuterNumberSerialize") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.fake_outer_number_serialize(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("FakeOuterStringSerialize") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.fake_outer_string_serialize(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        /* Disabled because there's no example.
        Some("TestBodyWithQueryParams") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_body_with_query_params(
                  "query_example".to_string(),
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        /* Disabled because there's no example.
        Some("TestClientModel") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_client_model(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        Some("TestEndpointParameters") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
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
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("TestEnumParameters") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_enum_parameters(
                  Some(&Vec::new()),
                  Some("enum_header_string_example".to_string()),
                  Some(&Vec::new()),
                  Some("enum_query_string_example".to_string()),
                  Some(56),
                  Some(1.2),
                  Some("enum_form_string_example".to_string())
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        /* Disabled because there's no example.
        Some("TestInlineAdditionalProperties") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_inline_additional_properties(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        Some("TestJsonFormData") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_json_form_data(
                  "param_example".to_string(),
                  "param2_example".to_string()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        /* Disabled because there's no example.
        Some("TestClassname") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.test_classname(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        /* Disabled because there's no example.
        Some("AddPet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.add_pet(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        Some("DeletePet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.delete_pet(
                  789,
                  Some("api_key_example".to_string())
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("FindPetsByStatus") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.find_pets_by_status(
                  &Vec::new()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("FindPetsByTags") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.find_pets_by_tags(
                  &Vec::new()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("GetPetById") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.get_pet_by_id(
                  789
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        /* Disabled because there's no example.
        Some("UpdatePet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.update_pet(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        Some("UpdatePetWithForm") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.update_pet_with_form(
                  789,
                  Some("name_example".to_string()),
                  Some("status_example".to_string())
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("UploadFile") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.upload_file(
                  789,
                  Some("additional_metadata_example".to_string()),
                  Some(swagger::ByteArray(Vec::from("BINARY_DATA_HERE")))
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("DeleteOrder") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.delete_order(
                  "order_id_example".to_string()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("GetInventory") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.get_inventory(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("GetOrderById") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.get_order_by_id(
                  789
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        /* Disabled because there's no example.
        Some("PlaceOrder") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.place_order(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        /* Disabled because there's no example.
        Some("CreateUser") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.create_user(
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        Some("CreateUsersWithArrayInput") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.create_users_with_array_input(
                  &Vec::new()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("CreateUsersWithListInput") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.create_users_with_list_input(
                  &Vec::new()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("DeleteUser") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.delete_user(
                  "username_example".to_string()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("GetUserByName") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.get_user_by_name(
                  "username_example".to_string()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("LoginUser") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.login_user(
                  "username_example".to_string(),
                  "password_example".to_string()
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("LogoutUser") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.logout_user(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        /* Disabled because there's no example.
        Some("UpdateUser") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.update_user(
                  "username_example".to_string(),
                  ???
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */

        _ => {
            panic!("Invalid operation provided")
        }
    }
}
