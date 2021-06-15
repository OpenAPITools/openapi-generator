#![allow(missing_docs, unused_variables, trivial_casts)]


#[allow(unused_imports)]
use futures::{future, Stream, stream};
#[allow(unused_imports)]
use petstore_with_fake_endpoints_models_for_testing::{Api, ApiNoContext, Client, ContextWrapperExt, models,
                      TestSpecialTagsResponse,
                      Call123exampleResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
                      FakeResponseWithNumericalDescriptionResponse,
                      HyphenParamResponse,
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
                      UpdateUserResponse,
                     };
use clap::{App, Arg};

#[allow(unused_imports)]
use log::info;

// swagger::Has may be unused if there are no examples
#[allow(unused_imports)]
use swagger::{AuthData, ContextBuilder, EmptyContext, Has, Push, XSpanIdString};

type ClientContext = swagger::make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanIdString);

// rt may be unused if there are no examples
#[allow(unused_mut)]
fn main() {
    env_logger::init();

    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[
                "Call123example",
                "FakeOuterBooleanSerialize",
                "FakeOuterCompositeSerialize",
                "FakeOuterNumberSerialize",
                "FakeOuterStringSerialize",
                "FakeResponseWithNumericalDescription",
                "HyphenParam",
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

    let context: ClientContext =
        swagger::make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanIdString::default());

    let mut client : Box<dyn ApiNoContext<ClientContext>> = if matches.is_present("https") {
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

    match matches.value_of("operation") {
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
        Some("HyphenParam") => {
            let result = rt.block_on(client.hyphen_param(
                  "hyphen_param_example".to_string()
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
                  Some("enum_header_string_example".to_string()),
                  Some(&Vec::new()),
                  Some("enum_query_string_example".to_string()),
                  Some(56),
                  Some(1.2),
                  Some("enum_form_string_example".to_string())
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
        Some("DeletePet") => {
            let result = rt.block_on(client.delete_pet(
                  789,
                  Some("api_key_example".to_string())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
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
        Some("GetPetById") => {
            let result = rt.block_on(client.get_pet_by_id(
                  789
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
        Some("DeleteOrder") => {
            let result = rt.block_on(client.delete_order(
                  "order_id_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("GetInventory") => {
            let result = rt.block_on(client.get_inventory(
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
        Some("PlaceOrder") => {
            let result = rt.block_on(client.place_order(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
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
