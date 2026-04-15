/// Describes the result of uploading an image resource

// ApiResponse.mo

module {
    // User-facing type: what application code uses
    public type ApiResponse = {
        code : ?Int;
        type_ : ?Text;
        message : ?Text;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer ApiResponse type
        public type JSON = {
            code : ?Int;
            type_ : ?Text;
            message : ?Text;
        };

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : ApiResponse) : JSON = value;

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?ApiResponse = ?json;
    }
}
