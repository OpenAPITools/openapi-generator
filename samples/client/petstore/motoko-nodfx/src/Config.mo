// Config.mo — shared configuration types and default config for OpenAPI Petstore

module {
    public type Auth = {
        #bearer : Text;
        #apiKey : Text;
        #basicAuth : { user : Text; password : Text };
    };

    type http_header = { name : Text; value : Text };
    type http_request_result = { status : Nat; headers : [http_header]; body : Blob };

    public type Config = {
        baseUrl : Text;
        auth : ?Auth;
        max_response_bytes : ?Nat64;
        transform : ?{
            function : shared query ({ response : http_request_result; context : Blob }) -> async http_request_result;
            context : Blob;
        };
        is_replicated : ?Bool;
        cycles : Nat;
    };

    /// Default configuration for OpenAPI Petstore.
    /// Customize with record update syntax:
    ///   { defaultConfig with auth = ?#bearer "my-token" }
    public let defaultConfig : Config = {
        baseUrl = "http://petstore.swagger.io/v2";
        auth = null;
        max_response_bytes = null;
        transform = null;
        is_replicated = null;
        cycles = 30_000_000_000;
    };
}
