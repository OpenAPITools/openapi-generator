/// A User who is purchasing from the pet store

// User.mo

module {
    // User-facing type: what application code uses
    public type User = {
        id : ?Int;
        username : ?Text;
        firstName : ?Text;
        lastName : ?Text;
        email : ?Text;
        password : ?Text;
        phone : ?Text;
        /// User Status
        userStatus : ?Int;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer User type
        public type JSON = {
            id : ?Int;
            username : ?Text;
            firstName : ?Text;
            lastName : ?Text;
            email : ?Text;
            password : ?Text;
            phone : ?Text;
            userStatus : ?Int;
        };

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : User) : JSON = value;

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?User = ?json;
    }
}
