/// Order Status

// OrderStatus.mo
/// Enum values: #placed, #approved, #delivered

module {
    // User-facing type: type-safe variants for application code
    public type OrderStatus = {
        #placed;
        #approved;
        #delivered;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer OrderStatus type
        public type JSON = Text;

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : OrderStatus) : JSON =
            switch (value) {
                case (#placed) "placed";
                case (#approved) "approved";
                case (#delivered) "delivered";
            };

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?OrderStatus =
            switch (json) {
                case "placed" ?#placed;
                case "approved" ?#approved;
                case "delivered" ?#delivered;
                case _ null;
            };
    }
}
