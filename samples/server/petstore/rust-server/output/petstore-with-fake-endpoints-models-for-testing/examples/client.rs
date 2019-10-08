#![allow(missing_docs, unused_variables, trivial_casts)]
use openapi_context::{make_context_ty, make_context, ContextBuilder, EmptyContext, XSpanId, Has, Push, AuthData};

#[allow(unused_imports)]
use petstore_with_fake_endpoints_models_for_testing::{ApiNoContext, ContextWrapperExt,
                      ApiError,
                      TestSpecialTagsResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
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
                      UpdateUserResponse
                     };
use clap::{App, Arg, ArgMatches};
use hyper_rustls::HttpsConnector;
use hyper::client::HttpConnector;
use petstore_with_fake_endpoints_models_for_testing::Client;

async fn run_operation<'a, C>(matches: ArgMatches<'a>, client: Client<C>)
    where C: hyper::client::connect::Connect + Clone + Send + Sync + 'static
{
    let context: make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanId) =
            make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanId(uuid::Uuid::new_v4().to_string()));
    let mut client = client.with_context(context);

    match matches.value_of("operation") {

        // Disabled because there's no example.
        // Some("TestSpecialTags") => {
        //     let result = client.test_special_tags(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        Some("FakeOuterBooleanSerialize") => {
            let result = client.fake_outer_boolean_serialize(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("FakeOuterCompositeSerialize") => {
            let result = client.fake_outer_composite_serialize(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("FakeOuterNumberSerialize") => {
            let result = client.fake_outer_number_serialize(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("FakeOuterStringSerialize") => {
            let result = client.fake_outer_string_serialize(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("HyphenParam") => {
            let result = client.hyphen_param("hyphen_param_example".to_string()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        // Disabled because there's no example.
        // Some("TestBodyWithQueryParams") => {
        //     let result = client.test_body_with_query_params("query_example".to_string(), ???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        // Disabled because there's no example.
        // Some("TestClientModel") => {
        //     let result = client.test_client_model(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        Some("TestEndpointParameters") => {
            let result = client.test_endpoint_parameters(8.14, 1.2, "pattern_without_delimiter_example".to_string(), openapi_context::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE")), Some(56), Some(56), Some(789), Some(3.4), Some("string_example".to_string()), Some(openapi_context::ByteArray(Vec::from("BINARY_DATA_HERE"))), None, None, Some("password_example".to_string()), Some("callback_example".to_string())).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("TestEnumParameters") => {
            let result = client.test_enum_parameters(Some(&Vec::new()), Some("enum_header_string_example".to_string()), Some(&Vec::new()), Some("enum_query_string_example".to_string()), Some(56), Some(1.2), Some("enum_form_string_example".to_string())).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        // Disabled because there's no example.
        // Some("TestInlineAdditionalProperties") => {
        //     let result = client.test_inline_additional_properties(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        Some("TestJsonFormData") => {
            let result = client.test_json_form_data("param_example".to_string(), "param2_example".to_string()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        // Disabled because there's no example.
        // Some("TestClassname") => {
        //     let result = client.test_classname(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        // Disabled because there's no example.
        // Some("AddPet") => {
        //     let result = client.add_pet(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        Some("DeletePet") => {
            let result = client.delete_pet(789, Some("api_key_example".to_string())).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("FindPetsByStatus") => {
            let result = client.find_pets_by_status(&Vec::new()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("FindPetsByTags") => {
            let result = client.find_pets_by_tags(&Vec::new()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("GetPetById") => {
            let result = client.get_pet_by_id(789).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        // Disabled because there's no example.
        // Some("UpdatePet") => {
        //     let result = client.update_pet(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        Some("UpdatePetWithForm") => {
            let result = client.update_pet_with_form(789, Some("name_example".to_string()), Some("status_example".to_string())).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("UploadFile") => {
            let result = client.upload_file(789, Some("additional_metadata_example".to_string()), Some(openapi_context::ByteArray(Vec::from("BINARY_DATA_HERE")))).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("DeleteOrder") => {
            let result = client.delete_order("order_id_example".to_string()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("GetInventory") => {
            let result = client.get_inventory().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("GetOrderById") => {
            let result = client.get_order_by_id(789).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        // Disabled because there's no example.
        // Some("PlaceOrder") => {
        //     let result = client.place_order(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        // Disabled because there's no example.
        // Some("CreateUser") => {
        //     let result = client.create_user(???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        Some("CreateUsersWithArrayInput") => {
            let result = client.create_users_with_array_input(&Vec::new()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("CreateUsersWithListInput") => {
            let result = client.create_users_with_list_input(&Vec::new()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("DeleteUser") => {
            let result = client.delete_user("username_example".to_string()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("GetUserByName") => {
            let result = client.get_user_by_name("username_example".to_string()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("LoginUser") => {
            let result = client.login_user("username_example".to_string(), "password_example".to_string()).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("LogoutUser") => {
            let result = client.logout_user().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        // Disabled because there's no example.
        // Some("UpdateUser") => {
        //     let result = client.update_user("username_example".to_string(), ???).await;
        //     println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
        //  },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}

#[tokio::main]
async fn main() {
    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[
    "FakeOuterBooleanSerialize",
    "FakeOuterCompositeSerialize",
    "FakeOuterNumberSerialize",
    "FakeOuterStringSerialize",
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
    if matches.is_present("https") {
        // Using Simple HTTPS
        let cli = Client::<HttpsConnector<HttpConnector>>::try_new_https(&base_url, "examples/ca.pem")
            .expect("Failed to create HTTPS client");
        run_operation(matches, cli).await
    } else {
        // Using HTTP
        let cli = Client::<HttpConnector>::try_new_http(&base_url)
            .expect("Failed to create HTTP client");
        run_operation(matches, cli).await
    }
}

