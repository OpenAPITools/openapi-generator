/// pet status in the store

// PetStatus.mo
/// Enum values: #available, #pending, #sold

module {
    // User-facing type: type-safe variants for application code
    public type PetStatus = {
        #available;
        #pending;
        #sold;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer PetStatus type
        public type JSON = Text;

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : PetStatus) : JSON =
            switch (value) {
                case (#available) "available";
                case (#pending) "pending";
                case (#sold) "sold";
            };

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?PetStatus =
            switch (json) {
                case "available" ?#available;
                case "pending" ?#pending;
                case "sold" ?#sold;
                case _ null;
            };
    }
}
