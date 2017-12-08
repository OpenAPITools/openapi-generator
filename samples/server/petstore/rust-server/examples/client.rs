#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate petstore_api;
#[allow(unused_extern_crates)]
extern crate futures;
#[allow(unused_extern_crates)]
extern crate swagger;
#[allow(unused_extern_crates)]
extern crate uuid;
extern crate clap;

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use petstore_api::{ApiNoContext, ContextWrapperExt,
                      ApiError,
                      TestSpecialTagsResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
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
    let client = if is_https {
        // Using Simple HTTPS
        petstore_api::Client::try_new_https(&base_url, "examples/ca.pem")
            .expect("Failed to create HTTPS client")
    } else {
        // Using HTTP
        petstore_api::Client::try_new_http(&base_url)
            .expect("Failed to create HTTP client")
    };

    // Using a non-default `Context` is not required; this is just an example!
    let client = client.with_context(petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string()));

    match matches.value_of("operation") {

        // Disabled because there's no example.
        // Some("TestSpecialTags") => {
        //     let result = client.test_special_tags(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        Some("FakeOuterBooleanSerialize") => {
            let result = client.fake_outer_boolean_serialize(None).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("FakeOuterCompositeSerialize") => {
            let result = client.fake_outer_composite_serialize(None).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("FakeOuterNumberSerialize") => {
            let result = client.fake_outer_number_serialize(None).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("FakeOuterStringSerialize") => {
            let result = client.fake_outer_string_serialize(None).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        // Disabled because there's no example.
        // Some("TestClientModel") => {
        //     let result = client.test_client_model(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        Some("TestEndpointParameters") => {
            let result = client.test_endpoint_parameters(8.14, 1.2, "pattern_without_delimiter_example".to_string(), swagger::ByteArray(Vec::from("B")), Some(56), Some(56), Some(789), Some(3.4), Some("string_example".to_string()), Some(swagger::ByteArray(Vec::from("B"))), None, None, Some("password_example".to_string()), Some("callback_example".to_string())).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("TestEnumParameters") => {
            let result = client.test_enum_parameters(Some(&Vec::new()), Some("enum_form_string_example".to_string()), Some(&Vec::new()), Some("enum_header_string_example".to_string()), Some(&Vec::new()), Some("enum_query_string_example".to_string()), Some(56), Some(1.2)).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        // Disabled because there's no example.
        // Some("TestInlineAdditionalProperties") => {
        //     let result = client.test_inline_additional_properties(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        Some("TestJsonFormData") => {
            let result = client.test_json_form_data("param_example".to_string(), "param2_example".to_string()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        // Disabled because there's no example.
        // Some("TestClassname") => {
        //     let result = client.test_classname(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        // Disabled because there's no example.
        // Some("AddPet") => {
        //     let result = client.add_pet(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        Some("DeletePet") => {
            let result = client.delete_pet(789, Some("api_key_example".to_string())).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("FindPetsByStatus") => {
            let result = client.find_pets_by_status(&Vec::new()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("FindPetsByTags") => {
            let result = client.find_pets_by_tags(&Vec::new()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("GetPetById") => {
            let result = client.get_pet_by_id(789).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        // Disabled because there's no example.
        // Some("UpdatePet") => {
        //     let result = client.update_pet(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        Some("UpdatePetWithForm") => {
            let result = client.update_pet_with_form(789, Some("name_example".to_string()), Some("status_example".to_string())).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("UploadFile") => {
            let result = client.upload_file(789, Some("additional_metadata_example".to_string()), Box::new(future::ok(Some(Box::new(stream::once(Ok(b"hello".to_vec()))) as Box<Stream<Item=_, Error=_> + Send>))) as Box<Future<Item=_, Error=_> + Send>).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("DeleteOrder") => {
            let result = client.delete_order("order_id_example".to_string()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("GetInventory") => {
            let result = client.get_inventory().wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("GetOrderById") => {
            let result = client.get_order_by_id(789).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        // Disabled because there's no example.
        // Some("PlaceOrder") => {
        //     let result = client.place_order(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        // Disabled because there's no example.
        // Some("CreateUser") => {
        //     let result = client.create_user(???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        Some("CreateUsersWithArrayInput") => {
            let result = client.create_users_with_array_input(&Vec::new()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("CreateUsersWithListInput") => {
            let result = client.create_users_with_list_input(&Vec::new()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("DeleteUser") => {
            let result = client.delete_user("username_example".to_string()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("GetUserByName") => {
            let result = client.get_user_by_name("username_example".to_string()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("LoginUser") => {
            let result = client.login_user("username_example".to_string(), "password_example".to_string()).wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        Some("LogoutUser") => {
            let result = client.logout_user().wait();
            println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
         },

        // Disabled because there's no example.
        // Some("UpdateUser") => {
        //     let result = client.update_user("username_example".to_string(), ???).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, client.context().x_span_id.clone().unwrap_or(String::from("<none>")));
        //  },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}

