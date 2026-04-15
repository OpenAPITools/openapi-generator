
// FindPetsByStatusStatusParameterInner.mo
/// Enum values: #available, #pending, #sold

module {
    // User-facing type: type-safe variants for application code
    public type FindPetsByStatusStatusParameterInner = {
        #available;
        #pending;
        #sold;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer FindPetsByStatusStatusParameterInner type
        public type JSON = Text;

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : FindPetsByStatusStatusParameterInner) : JSON =
            switch (value) {
                case (#available) "available";
                case (#pending) "pending";
                case (#sold) "sold";
            };

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?FindPetsByStatusStatusParameterInner =
            switch (json) {
                case "available" ?#available;
                case "pending" ?#pending;
                case "sold" ?#sold;
                case _ null;
            };
    }
}
