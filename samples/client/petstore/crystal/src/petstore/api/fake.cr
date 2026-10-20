require "json"

module Petstore
  module Api
  class Fake
    def initialize(@conn : Connection); end

    # test referenced additionalProperties 
    def additional_properties_reference(request_body : Hash(String, JSON::Any)) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/fake/additionalProperties-reference",
        body: request_body,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    #  for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    def big_decimal_map() : Response(Petstore::FakeBigDecimalMap200Response)
      @conn.request(Petstore::FakeBigDecimalMap200Response,
        method: :GET,
        path: "/fake/BigDecimalMap",
        accept: %w[*/*],
        raw: true,
        auth: %w[])
    end

    #  For this test, the body has to be a binary file.
    def body_with_binary(body : ::File) : Response(Nil)
      @conn.request(Nil,
        method: :PUT,
        path: "/fake/body-with-binary",
        body: body,
        accept: %w[],
        content_type: %w[image/png],
        auth: %w[])
    end

    #  For this test, the body for this request must reference a schema named &#x60;File&#x60;.
    def body_with_file_schema(file_schema_test_class : Petstore::FileSchemaTestClass) : Response(Nil)
      @conn.request(Nil,
        method: :PUT,
        path: "/fake/body-with-file-schema",
        body: file_schema_test_class,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # 
    def body_with_query_params(user : Petstore::User, *, query : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :PUT,
        path: "/fake/body-with-query-params",
        body: user,
        query: { "query" => query },
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # Fake endpoint to test group parameters (optional) Fake endpoint to test group parameters (optional)
    def bulk_destroy(*, required_boolean_group : Bool? = nil, boolean_group : Bool? = nil, required_string_group : Int32? = nil, required_int64_group : Int64? = nil, string_group : Int32? = nil, int64_group : Int64? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :DELETE,
        path: "/fake",
        header: { "required_boolean_group" => required_boolean_group.try &.to_s, "boolean_group" => boolean_group.try &.to_s },
        query: { "required_string_group" => required_string_group, "required_int64_group" => required_int64_group, "string_group" => string_group, "int64_group" => int64_group },
        accept: %w[],
        auth: %w[bearer_test])
    end

    # To test \&quot;client\&quot; model To test \&quot;client\&quot; model
    def bulk_partial_update(model_client : Petstore::ModelClient) : Response(Petstore::ModelClient)
      @conn.request(Petstore::ModelClient,
        method: :PATCH,
        path: "/fake",
        body: model_client,
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[])
    end

    # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    def create(integer : Int32? = nil, int32 : Int32? = nil, int64 : Int64? = nil, number : Float64? = nil, float : Float32? = nil, double : Float64? = nil, string : String? = nil, pattern_without_delimiter : String? = nil, byte : String? = nil, binary : ::File? = nil, date : Time? = nil, date_time : Time? = nil, password : String? = nil, callback : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/fake",
        form: Hash(String, Crest::ParamsValue){ "integer" => integer, "int32" => int32, "int64" => int64, "number" => number, "float" => float, "double" => double, "string" => string, "pattern_without_delimiter" => pattern_without_delimiter, "byte" => byte, "binary" => binary, "date" => date, "dateTime" => date_time, "password" => password, "callback" => callback },
        accept: %w[],
        auth: %w[http_basic_test])
    end

    # Health check endpoint
    def health_get() : Response(Petstore::HealthCheckResult)
      @conn.request(Petstore::HealthCheckResult,
        method: :GET,
        path: "/fake/health",
        accept: %w[application/json],
        auth: %w[])
    end

    # test http signature authentication
    def http_signature_test(pet : Petstore::Pet, *, header_1 : String? = nil, query_1 : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :GET,
        path: "/fake/http-signature-test",
        body: pet,
        header: { "header_1" => header_1.try &.to_s },
        query: { "query_1" => query_1 },
        accept: %w[],
        content_type: %w[application/json application/xml],
        auth: %w[http_signature_test])
    end

    # test inline additionalProperties 
    def inline_additional_properties(request_body : Hash(String, String)) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/fake/inline-additionalProperties",
        body: request_body,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # test inline free-form additionalProperties 
    def inline_freeform_additional_properties(test_inline_freeform_additional_properties_request : Petstore::TestInlineFreeformAdditionalPropertiesRequest) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/fake/inline-freeform-additionalProperties",
        body: test_inline_freeform_additional_properties_request,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # test json serialization of form data 
    def json_form_data(param : String? = nil, param2 : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :GET,
        path: "/fake/jsonFormData",
        form: Hash(String, Crest::ParamsValue){ "param" => param, "param2" => param2 },
        accept: %w[],
        auth: %w[])
    end

    # To test enum parameters To test enum parameters
    def list(enum_form_string_array : Array(String)? = nil, enum_form_string : String? = nil, *, enum_header_string_array : Array(String)? = nil, enum_header_string : String? = nil, enum_query_string_array : Array(String)? = nil, enum_query_string : String? = nil, enum_query_integer : Int32? = nil, enum_query_double : Float64? = nil, enum_query_model_array : Array(Petstore::EnumClass)? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :GET,
        path: "/fake",
        header: { "enum_header_string_array" => enum_header_string_array.try &.to_s, "enum_header_string" => enum_header_string.try &.to_s },
        query: { "enum_query_string_array" => enum_query_string_array, "enum_query_string" => enum_query_string, "enum_query_integer" => enum_query_integer, "enum_query_double" => enum_query_double, "enum_query_model_array" => enum_query_model_array },
        form: Hash(String, Crest::ParamsValue){ "enum_form_string_array" => enum_form_string_array, "enum_form_string" => enum_form_string },
        accept: %w[],
        auth: %w[])
    end

    # test nullable parent property 
    def nullable(child_with_nullable : Petstore::ChildWithNullable) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/fake/nullable",
        body: child_with_nullable,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    #  Test serialization of outer boolean types
    def outer_boolean_serialize(body : Bool? = nil) : Response(Bool)
      @conn.request(Bool,
        method: :POST,
        path: "/fake/outer/boolean",
        body: body,
        accept: %w[*/*],
        content_type: %w[application/json],
        raw: true,
        auth: %w[])
    end

    #  Test serialization of object with outer number type
    def outer_composite_serialize(outer_composite : Petstore::OuterComposite? = nil) : Response(Petstore::OuterComposite)
      @conn.request(Petstore::OuterComposite,
        method: :POST,
        path: "/fake/outer/composite",
        body: outer_composite,
        accept: %w[*/*],
        content_type: %w[application/json],
        raw: true,
        auth: %w[])
    end

    #  Test serialization of outer number types
    def outer_number_serialize(body : Float64? = nil) : Response(Float64)
      @conn.request(Float64,
        method: :POST,
        path: "/fake/outer/number",
        body: body,
        accept: %w[*/*],
        content_type: %w[application/json],
        raw: true,
        auth: %w[])
    end

    #  Test serialization of outer string types
    def outer_string_serialize(body : String? = nil) : Response(String)
      @conn.request(String,
        method: :POST,
        path: "/fake/outer/string",
        body: body,
        accept: %w[*/*],
        content_type: %w[application/json],
        raw: true,
        auth: %w[])
    end

    #  Test serialization of enum (int) properties with examples
    def property_enum_integer_serialize(outer_object_with_enum_property : Petstore::OuterObjectWithEnumProperty) : Response(Petstore::OuterObjectWithEnumProperty)
      @conn.request(Petstore::OuterObjectWithEnumProperty,
        method: :POST,
        path: "/fake/property/enum-int",
        body: outer_object_with_enum_property,
        accept: %w[*/*],
        content_type: %w[application/json],
        raw: true,
        auth: %w[])
    end

    # test referenced string map 
    def string_map_reference(request_body : Hash(String, String)) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/fake/stringMap-reference",
        body: request_body,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    #  To test the collection format in query parameters
    def test_query_parameters(*, pipe : Array(String)? = nil, ioutil : Array(String)? = nil, http : Array(String)? = nil, url : Array(String)? = nil, context : Array(String)? = nil, language : Hash(String, String)? = nil, allow_empty : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :PUT,
        path: "/fake/test-query-parameters",
        query: { "pipe" => pipe.try(&.map(&.to_s).join("|")), "ioutil" => ioutil.try(&.map(&.to_s).join(",")), "http" => http.try(&.map(&.to_s).join(" ")), "url" => url.try(&.map(&.to_s).join(",")), "context" => context, "language" => language, "allowEmpty" => allow_empty },
        accept: %w[],
        auth: %w[])
    end

    # uploads an image (required) 
    def upload_image_with_required_file(pet_id : Int64, additional_metadata : String? = nil, required_file : ::File? = nil) : Response(Petstore::ApiResponse)
      @conn.request(Petstore::ApiResponse,
        method: :POST,
        path: "/fake/{petId}/uploadImageWithRequiredFile".sub("{petId}", Petstore.enc(pet_id)),
        form: Hash(String, Crest::ParamsValue){ "additionalMetadata" => additional_metadata, "requiredFile" => required_file },
        accept: %w[application/json],
        auth: %w[petstore_auth])
    end
  end
  end

end
