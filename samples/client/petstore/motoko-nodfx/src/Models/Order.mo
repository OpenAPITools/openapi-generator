/// An order for a pets from the pet store

import { type OrderStatus; JSON = OrderStatus } "./OrderStatus";

// Order.mo

module {
    // User-facing type: what application code uses
    public type Order = {
        id : ?Int;
        petId : ?Int;
        quantity : ?Int;
        shipDate : ?Text;
        status : ?OrderStatus;
        complete : ?Bool;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer Order type
        public type JSON = {
            id : ?Int;
            petId : ?Int;
            quantity : ?Int;
            shipDate : ?Text;
            status : ?OrderStatus.JSON;
            complete : ?Bool;
        };

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : Order) : JSON = { value with
            status = do ? { OrderStatus.toJSON(value.status!) };
        };

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?Order {
            ?{ json with
                status = do ? { OrderStatus.fromJSON(json.status!)! };
            }
        };
    }
}
