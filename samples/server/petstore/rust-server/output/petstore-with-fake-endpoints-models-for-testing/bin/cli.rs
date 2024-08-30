//! CLI tool driving the API client
use anyhow::{anyhow, Context, Result};
use dialoguer::Confirm;
use log::{debug, info};
// models may be unused if all inputs are primitive types
#[allow(unused_imports)]
use petstore_with_fake_endpoints_models_for_testing::{
    models, ApiNoContext, Client, ContextWrapperExt,
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
use simple_logger::SimpleLogger;
use structopt::StructOpt;
use swagger::{AuthData, ContextBuilder, EmptyContext, Push, XSpanIdString};

type ClientContext = swagger::make_context_ty!(
    ContextBuilder,
    EmptyContext,
    Option<AuthData>,
    XSpanIdString
);

#[derive(StructOpt, Debug)]
#[structopt(
    name = "OpenAPI Petstore",
    version = "1.0.0",
    about = "CLI access to OpenAPI Petstore"
)]
struct Cli {
    #[structopt(subcommand)]
    operation: Operation,

    /// Address or hostname of the server hosting this API, including optional port
    #[structopt(short = "a", long, default_value = "http://localhost")]
    server_address: String,

    /// Path to the client private key if using client-side TLS authentication
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[structopt(long, requires_all(&["client-certificate", "server-certificate"]))]
    client_key: Option<String>,

    /// Path to the client's public certificate associated with the private key
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[structopt(long, requires_all(&["client-key", "server-certificate"]))]
    client_certificate: Option<String>,

    /// Path to CA certificate used to authenticate the server
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[structopt(long)]
    server_certificate: Option<String>,

    /// If set, write output to file instead of stdout
    #[structopt(short, long)]
    output_file: Option<String>,

    #[structopt(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,

    /// Don't ask for any confirmation prompts
    #[allow(dead_code)]
    #[structopt(short, long)]
    force: bool,

    /// Bearer token if used for authentication
    #[structopt(env = "PETSTORE_WITH_FAKE_ENDPOINTS_MODELS_FOR_TESTING_BEARER_TOKEN", hide_env_values = true)]
    bearer_token: Option<String>,
}

#[derive(StructOpt, Debug)]
enum Operation {
    /// To test special tags
    TestSpecialTags {
        /// client model
        #[structopt(parse(try_from_str = parse_json))]
        body: models::Client,
    },
    Call123example {
    },
    FakeOuterBooleanSerialize {
        /// Input boolean as post body
        #[structopt(parse(try_from_str = parse_json))]
        body: Option<models::OuterBoolean>,
    },
    FakeOuterCompositeSerialize {
        /// Input composite as post body
        #[structopt(parse(try_from_str = parse_json))]
        body: Option<models::OuterComposite>,
    },
    FakeOuterNumberSerialize {
        /// Input number as post body
        #[structopt(parse(try_from_str = parse_json))]
        body: Option<models::OuterNumber>,
    },
    FakeOuterStringSerialize {
        /// Input string as post body
        #[structopt(parse(try_from_str = parse_json))]
        body: Option<models::OuterString>,
    },
    FakeResponseWithNumericalDescription {
    },
    TestBodyWithQueryParams {
        query: String,
        #[structopt(parse(try_from_str = parse_json))]
        body: models::User,
    },
    /// To test \"client\" model
    TestClientModel {
        /// client model
        #[structopt(parse(try_from_str = parse_json))]
        body: models::Client,
    },
    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    TestEndpointParameters {
        /// None
        number: f64,
        /// None
        double: f64,
        /// None
        pattern_without_delimiter: String,
        /// None
        #[structopt(parse(try_from_str = parse_json))]
        byte: swagger::ByteArray,
        /// None
        integer: Option<i32>,
        /// None
        int32: Option<i32>,
        /// None
        int64: Option<i64>,
        /// None
        float: Option<f32>,
        /// None
        string: Option<String>,
        /// None
        #[structopt(parse(try_from_str = parse_json))]
        binary: Option<swagger::ByteArray>,
        /// None
        date: Option<chrono::naive::NaiveDate>,
        /// None
        date_time: Option<chrono::DateTime::<chrono::Utc>>,
        /// None
        password: Option<String>,
        /// None
        callback: Option<String>,
    },
    /// To test enum parameters
    TestEnumParameters {
        /// Header parameter enum test (string array)
        #[structopt(parse(try_from_str = parse_json), long)]
        enum_header_string_array: Option<Vec<models::TestEnumParametersEnumHeaderStringArrayParameterInner>>,
        /// Header parameter enum test (string)
        #[structopt(parse(try_from_str = parse_json))]
        enum_header_string: Option<models::TestEnumParametersEnumHeaderStringParameter>,
        /// Query parameter enum test (string array)
        #[structopt(parse(try_from_str = parse_json), long)]
        enum_query_string_array: Option<Vec<models::TestEnumParametersEnumHeaderStringArrayParameterInner>>,
        /// Query parameter enum test (string)
        #[structopt(parse(try_from_str = parse_json))]
        enum_query_string: Option<models::TestEnumParametersEnumHeaderStringParameter>,
        /// Query parameter enum test (double)
        #[structopt(parse(try_from_str = parse_json))]
        enum_query_integer: Option<models::TestEnumParametersEnumQueryIntegerParameter>,
        /// Query parameter enum test (double)
        #[structopt(parse(try_from_str = parse_json))]
        enum_query_double: Option<models::TestEnumParametersEnumQueryDoubleParameter>,
        #[structopt(parse(try_from_str = parse_json))]
        enum_form_string: Option<models::TestEnumParametersRequestEnumFormString>,
    },
    /// test inline additionalProperties
    TestInlineAdditionalProperties {
        /// request body
        #[structopt(parse(try_from_str = parse_json))]
        param: std::collections::HashMap<String, String>,
    },
    /// test json serialization of form data
    TestJsonFormData {
        /// field1
        param: String,
        /// field2
        param2: String,
    },
    HyphenParam {
        /// Parameter with hyphen in name
        hyphen_param: String,
    },
    /// To test class name in snake case
    TestClassname {
        /// client model
        #[structopt(parse(try_from_str = parse_json))]
        body: models::Client,
    },
    /// Add a new pet to the store
    AddPet {
        /// Pet object that needs to be added to the store
        #[structopt(parse(try_from_str = parse_json))]
        body: models::Pet,
    },
    /// Finds Pets by status
    FindPetsByStatus {
        /// Status values that need to be considered for filter
        #[structopt(parse(try_from_str = parse_json), long)]
        status: Vec<models::FindPetsByStatusStatusParameterInner>,
    },
    /// Finds Pets by tags
    FindPetsByTags {
        /// Tags to filter by
        #[structopt(parse(try_from_str = parse_json), long)]
        tags: Vec<String>,
    },
    /// Update an existing pet
    UpdatePet {
        /// Pet object that needs to be added to the store
        #[structopt(parse(try_from_str = parse_json))]
        body: models::Pet,
    },
    /// Deletes a pet
    DeletePet {
        /// Pet id to delete
        pet_id: i64,
        api_key: Option<String>,
    },
    /// Find pet by ID
    GetPetById {
        /// ID of pet to return
        pet_id: i64,
    },
    /// Updates a pet in the store with form data
    UpdatePetWithForm {
        /// ID of pet that needs to be updated
        pet_id: i64,
        /// Updated name of the pet
        name: Option<String>,
        /// Updated status of the pet
        status: Option<String>,
    },
    /// uploads an image
    UploadFile {
        /// ID of pet to update
        pet_id: i64,
        /// Additional data to pass to server
        additional_metadata: Option<String>,
        /// file to upload
        #[structopt(parse(try_from_str = parse_json))]
        file: Option<swagger::ByteArray>,
    },
    /// Returns pet inventories by status
    GetInventory {
    },
    /// Place an order for a pet
    PlaceOrder {
        /// order placed for purchasing the pet
        #[structopt(parse(try_from_str = parse_json))]
        body: models::Order,
    },
    /// Delete purchase order by ID
    DeleteOrder {
        /// ID of the order that needs to be deleted
        order_id: String,
    },
    /// Find purchase order by ID
    GetOrderById {
        /// ID of pet that needs to be fetched
        order_id: i64,
    },
    /// Create user
    CreateUser {
        /// Created user object
        #[structopt(parse(try_from_str = parse_json))]
        body: models::User,
    },
    /// Creates list of users with given input array
    CreateUsersWithArrayInput {
        /// List of user object
        #[structopt(parse(try_from_str = parse_json), long)]
        body: Vec<models::User>,
    },
    /// Creates list of users with given input array
    CreateUsersWithListInput {
        /// List of user object
        #[structopt(parse(try_from_str = parse_json), long)]
        body: Vec<models::User>,
    },
    /// Logs user into the system
    LoginUser {
        /// The user name for login
        username: String,
        /// The password for login in clear text
        password: String,
    },
    /// Logs out current logged in user session
    LogoutUser {
    },
    /// Delete user
    DeleteUser {
        /// The name that needs to be deleted
        username: String,
    },
    /// Get user by user name
    GetUserByName {
        /// The name that needs to be fetched. Use user1 for testing.
        username: String,
    },
    /// Updated user
    UpdateUser {
        /// name that need to be deleted
        username: String,
        /// Updated user object
        #[structopt(parse(try_from_str = parse_json))]
        body: models::User,
    },
}

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
fn create_client(args: &Cli, context: ClientContext) -> Result<Box<dyn ApiNoContext<ClientContext>>> {
    if args.client_certificate.is_some() {
        debug!("Using mutual TLS");
        let client = Client::try_new_https_mutual(
            &args.server_address,
            args.server_certificate.clone().unwrap(),
            args.client_key.clone().unwrap(),
            args.client_certificate.clone().unwrap(),
        )
        .context("Failed to create HTTPS client")?;
        Ok(Box::new(client.with_context(context)))
    } else if args.server_certificate.is_some() {
        debug!("Using TLS with pinned server certificate");
        let client =
            Client::try_new_https_pinned(&args.server_address, args.server_certificate.clone().unwrap())
                .context("Failed to create HTTPS client")?;
        Ok(Box::new(client.with_context(context)))
    } else {
        debug!("Using client without certificates");
        let client =
            Client::try_new(&args.server_address).context("Failed to create HTTP(S) client")?;
        Ok(Box::new(client.with_context(context)))
    }
}

#[cfg(any(target_os = "macos", target_os = "windows", target_os = "ios"))]
fn create_client(args: &Cli, context: ClientContext) -> Result<Box<dyn ApiNoContext<ClientContext>>> {
    let client =
        Client::try_new(&args.server_address).context("Failed to create HTTP(S) client")?;
    Ok(Box::new(client.with_context(context)))
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Cli::from_args();
    if let Some(log_level) = args.verbosity.log_level() {
        SimpleLogger::new().with_level(log_level.to_level_filter()).init()?;
    }

    debug!("Arguments: {:?}", &args);

    let mut auth_data: Option<AuthData> = None;

    if let Some(ref bearer_token) = args.bearer_token {
        debug!("Using bearer token");
        auth_data = Some(AuthData::bearer(bearer_token));
    }

    #[allow(trivial_casts)]
    let context = swagger::make_context!(
        ContextBuilder,
        EmptyContext,
        auth_data,
        XSpanIdString::default()
    );

    let client = create_client(&args, context)?;

    let result = match args.operation {
        Operation::TestSpecialTags {
            body,
        } => {
            info!("Performing a TestSpecialTags request");

            let result = client.test_special_tags(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestSpecialTagsResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::Call123example {
        } => {
            info!("Performing a Call123example request");

            let result = client.call123example(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Call123exampleResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::FakeOuterBooleanSerialize {
            body,
        } => {
            info!("Performing a FakeOuterBooleanSerialize request");

            let result = client.fake_outer_boolean_serialize(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FakeOuterBooleanSerializeResponse::OutputBoolean
                (body)
                => "OutputBoolean\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::FakeOuterCompositeSerialize {
            body,
        } => {
            info!("Performing a FakeOuterCompositeSerialize request");

            let result = client.fake_outer_composite_serialize(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FakeOuterCompositeSerializeResponse::OutputComposite
                (body)
                => "OutputComposite\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::FakeOuterNumberSerialize {
            body,
        } => {
            info!("Performing a FakeOuterNumberSerialize request");

            let result = client.fake_outer_number_serialize(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FakeOuterNumberSerializeResponse::OutputNumber
                (body)
                => "OutputNumber\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::FakeOuterStringSerialize {
            body,
        } => {
            info!("Performing a FakeOuterStringSerialize request");

            let result = client.fake_outer_string_serialize(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FakeOuterStringSerializeResponse::OutputString
                (body)
                => "OutputString\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::FakeResponseWithNumericalDescription {
        } => {
            info!("Performing a FakeResponseWithNumericalDescription request");

            let result = client.fake_response_with_numerical_description(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FakeResponseWithNumericalDescriptionResponse::Status200
                => "Status200\n".to_string()
                    ,
            }
        }
        Operation::TestBodyWithQueryParams {
            query,
            body,
        } => {
            info!("Performing a TestBodyWithQueryParams request");

            let result = client.test_body_with_query_params(
                query,
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestBodyWithQueryParamsResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::TestClientModel {
            body,
        } => {
            info!("Performing a TestClientModel request");

            let result = client.test_client_model(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestClientModelResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::TestEndpointParameters {
            number,
            double,
            pattern_without_delimiter,
            byte,
            integer,
            int32,
            int64,
            float,
            string,
            binary,
            date,
            date_time,
            password,
            callback,
        } => {
            info!("Performing a TestEndpointParameters request");

            let result = client.test_endpoint_parameters(
                number,
                double,
                pattern_without_delimiter,
                byte,
                integer,
                int32,
                int64,
                float,
                string,
                binary,
                date,
                date_time,
                password,
                callback,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestEndpointParametersResponse::InvalidUsernameSupplied
                => "InvalidUsernameSupplied\n".to_string()
                    ,
                TestEndpointParametersResponse::UserNotFound
                => "UserNotFound\n".to_string()
                    ,
            }
        }
        Operation::TestEnumParameters {
            enum_header_string_array,
            enum_header_string,
            enum_query_string_array,
            enum_query_string,
            enum_query_integer,
            enum_query_double,
            enum_form_string,
        } => {
            info!("Performing a TestEnumParameters request");

            let result = client.test_enum_parameters(
                enum_header_string_array.as_ref(),
                enum_header_string,
                enum_query_string_array.as_ref(),
                enum_query_string,
                enum_query_integer,
                enum_query_double,
                enum_form_string,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestEnumParametersResponse::InvalidRequest
                => "InvalidRequest\n".to_string()
                    ,
                TestEnumParametersResponse::NotFound
                => "NotFound\n".to_string()
                    ,
            }
        }
        Operation::TestInlineAdditionalProperties {
            param,
        } => {
            info!("Performing a TestInlineAdditionalProperties request");

            let result = client.test_inline_additional_properties(
                param,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestInlineAdditionalPropertiesResponse::SuccessfulOperation
                => "SuccessfulOperation\n".to_string()
                    ,
            }
        }
        Operation::TestJsonFormData {
            param,
            param2,
        } => {
            info!("Performing a TestJsonFormData request");

            let result = client.test_json_form_data(
                param,
                param2,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestJsonFormDataResponse::SuccessfulOperation
                => "SuccessfulOperation\n".to_string()
                    ,
            }
        }
        Operation::HyphenParam {
            hyphen_param,
        } => {
            info!("Performing a HyphenParam request on {:?}", (
                &hyphen_param
            ));

            let result = client.hyphen_param(
                hyphen_param,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                HyphenParamResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::TestClassname {
            body,
        } => {
            info!("Performing a TestClassname request");

            let result = client.test_classname(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TestClassnameResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::AddPet {
            body,
        } => {
            info!("Performing a AddPet request");

            let result = client.add_pet(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                AddPetResponse::InvalidInput
                => "InvalidInput\n".to_string()
                    ,
            }
        }
        Operation::FindPetsByStatus {
            status,
        } => {
            info!("Performing a FindPetsByStatus request");

            let result = client.find_pets_by_status(
                status.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FindPetsByStatusResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                FindPetsByStatusResponse::InvalidStatusValue
                => "InvalidStatusValue\n".to_string()
                    ,
            }
        }
        Operation::FindPetsByTags {
            tags,
        } => {
            info!("Performing a FindPetsByTags request");

            let result = client.find_pets_by_tags(
                tags.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FindPetsByTagsResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                FindPetsByTagsResponse::InvalidTagValue
                => "InvalidTagValue\n".to_string()
                    ,
            }
        }
        Operation::UpdatePet {
            body,
        } => {
            info!("Performing a UpdatePet request");

            let result = client.update_pet(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                UpdatePetResponse::InvalidIDSupplied
                => "InvalidIDSupplied\n".to_string()
                    ,
                UpdatePetResponse::PetNotFound
                => "PetNotFound\n".to_string()
                    ,
                UpdatePetResponse::ValidationException
                => "ValidationException\n".to_string()
                    ,
            }
        }
        Operation::DeletePet {
            pet_id,
            api_key,
        } => {
            prompt(args.force, "This will delete the given entry, are you sure?")?;
            info!("Performing a DeletePet request on {:?}", (
                &pet_id
            ));

            let result = client.delete_pet(
                pet_id,
                api_key,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                DeletePetResponse::InvalidPetValue
                => "InvalidPetValue\n".to_string()
                    ,
            }
        }
        Operation::GetPetById {
            pet_id,
        } => {
            info!("Performing a GetPetById request on {:?}", (
                &pet_id
            ));

            let result = client.get_pet_by_id(
                pet_id,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetPetByIdResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                GetPetByIdResponse::InvalidIDSupplied
                => "InvalidIDSupplied\n".to_string()
                    ,
                GetPetByIdResponse::PetNotFound
                => "PetNotFound\n".to_string()
                    ,
            }
        }
        Operation::UpdatePetWithForm {
            pet_id,
            name,
            status,
        } => {
            info!("Performing a UpdatePetWithForm request on {:?}", (
                &pet_id
            ));

            let result = client.update_pet_with_form(
                pet_id,
                name,
                status,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                UpdatePetWithFormResponse::InvalidInput
                => "InvalidInput\n".to_string()
                    ,
            }
        }
        Operation::UploadFile {
            pet_id,
            additional_metadata,
            file,
        } => {
            info!("Performing a UploadFile request on {:?}", (
                &pet_id
            ));

            let result = client.upload_file(
                pet_id,
                additional_metadata,
                file,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                UploadFileResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::GetInventory {
        } => {
            info!("Performing a GetInventory request");

            let result = client.get_inventory(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetInventoryResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::PlaceOrder {
            body,
        } => {
            info!("Performing a PlaceOrder request");

            let result = client.place_order(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                PlaceOrderResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                PlaceOrderResponse::InvalidOrder
                => "InvalidOrder\n".to_string()
                    ,
            }
        }
        Operation::DeleteOrder {
            order_id,
        } => {
            prompt(args.force, "This will delete the given entry, are you sure?")?;
            info!("Performing a DeleteOrder request on {:?}", (
                &order_id
            ));

            let result = client.delete_order(
                order_id,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                DeleteOrderResponse::InvalidIDSupplied
                => "InvalidIDSupplied\n".to_string()
                    ,
                DeleteOrderResponse::OrderNotFound
                => "OrderNotFound\n".to_string()
                    ,
            }
        }
        Operation::GetOrderById {
            order_id,
        } => {
            info!("Performing a GetOrderById request on {:?}", (
                &order_id
            ));

            let result = client.get_order_by_id(
                order_id,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetOrderByIdResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                GetOrderByIdResponse::InvalidIDSupplied
                => "InvalidIDSupplied\n".to_string()
                    ,
                GetOrderByIdResponse::OrderNotFound
                => "OrderNotFound\n".to_string()
                    ,
            }
        }
        Operation::CreateUser {
            body,
        } => {
            info!("Performing a CreateUser request");

            let result = client.create_user(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                CreateUserResponse::SuccessfulOperation
                => "SuccessfulOperation\n".to_string()
                    ,
            }
        }
        Operation::CreateUsersWithArrayInput {
            body,
        } => {
            info!("Performing a CreateUsersWithArrayInput request");

            let result = client.create_users_with_array_input(
                body.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                CreateUsersWithArrayInputResponse::SuccessfulOperation
                => "SuccessfulOperation\n".to_string()
                    ,
            }
        }
        Operation::CreateUsersWithListInput {
            body,
        } => {
            info!("Performing a CreateUsersWithListInput request");

            let result = client.create_users_with_list_input(
                body.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                CreateUsersWithListInputResponse::SuccessfulOperation
                => "SuccessfulOperation\n".to_string()
                    ,
            }
        }
        Operation::LoginUser {
            username,
            password,
        } => {
            info!("Performing a LoginUser request");

            let result = client.login_user(
                username,
                password,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                LoginUserResponse::SuccessfulOperation
                {
                    body,
                    x_rate_limit,
                    x_expires_after,
                }
                => "SuccessfulOperation\n".to_string()
                   +
                    &format!("body: {}\n", serde_json::to_string_pretty(&body)?) +
                    &format!(
                        "x_rate_limit: {}\n",
                        serde_json::to_string_pretty(&x_rate_limit)?
                    ) +
                    &format!(
                        "x_expires_after: {}\n",
                        serde_json::to_string_pretty(&x_expires_after)?
                    ),
                LoginUserResponse::InvalidUsername
                => "InvalidUsername\n".to_string()
                    ,
            }
        }
        Operation::LogoutUser {
        } => {
            info!("Performing a LogoutUser request");

            let result = client.logout_user(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                LogoutUserResponse::SuccessfulOperation
                => "SuccessfulOperation\n".to_string()
                    ,
            }
        }
        Operation::DeleteUser {
            username,
        } => {
            prompt(args.force, "This will delete the given entry, are you sure?")?;
            info!("Performing a DeleteUser request on {:?}", (
                &username
            ));

            let result = client.delete_user(
                username,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                DeleteUserResponse::InvalidUsernameSupplied
                => "InvalidUsernameSupplied\n".to_string()
                    ,
                DeleteUserResponse::UserNotFound
                => "UserNotFound\n".to_string()
                    ,
            }
        }
        Operation::GetUserByName {
            username,
        } => {
            info!("Performing a GetUserByName request on {:?}", (
                &username
            ));

            let result = client.get_user_by_name(
                username,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetUserByNameResponse::SuccessfulOperation
                (body)
                => "SuccessfulOperation\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                GetUserByNameResponse::InvalidUsernameSupplied
                => "InvalidUsernameSupplied\n".to_string()
                    ,
                GetUserByNameResponse::UserNotFound
                => "UserNotFound\n".to_string()
                    ,
            }
        }
        Operation::UpdateUser {
            username,
            body,
        } => {
            info!("Performing a UpdateUser request on {:?}", (
                &username
            ));

            let result = client.update_user(
                username,
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                UpdateUserResponse::InvalidUserSupplied
                => "InvalidUserSupplied\n".to_string()
                    ,
                UpdateUserResponse::UserNotFound
                => "UserNotFound\n".to_string()
                    ,
            }
        }
    };

    if let Some(output_file) = args.output_file {
        std::fs::write(output_file, result)?
    } else {
        println!("{}", result);
    }
    Ok(())
}

fn prompt(force: bool, text: &str) -> Result<()> {
    if force || Confirm::new().with_prompt(text).interact()? {
        Ok(())
    } else {
        Err(anyhow!("Aborting"))
    }
}

// May be unused if all inputs are primitive types
#[allow(dead_code)]
fn parse_json<'a, T: serde::de::Deserialize<'a>>(json_string: &'a str) -> Result<T> {
    serde_json::from_str(json_string).map_err(|err| anyhow!("Error parsing input: {}", err))
}
