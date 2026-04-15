/// A category for a pet

// Category.mo

module {
    // User-facing type: what application code uses
    public type Category = {
        id : ?Int;
        name : ?Text;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer Category type
        public type JSON = {
            id : ?Int;
            name : ?Text;
        };

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : Category) : JSON = value;

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?Category = ?json;
    }
}
