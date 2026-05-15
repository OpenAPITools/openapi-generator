/// Order Status
import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// OrderStatus.mo
/// Enum values: #placed, #approved, #delivered

module {
    public type OrderStatus = {
        #placed;
        #approved;
        #delivered;
    };

    public module JSON {
        public func toCandidValue(value : OrderStatus) : Candid.Candid =
            switch (value) {
                case (#placed) #Text("placed");
                case (#approved) #Text("approved");
                case (#delivered) #Text("delivered");
            };

        public func fromCandidValue(candid : Candid.Candid) : ?OrderStatus =
            switch (candid) {
                case (#Text("placed")) ?#placed;
                case (#Text("approved")) ?#approved;
                case (#Text("delivered")) ?#delivered;
                case _ null;
            };

        public func toText(value : OrderStatus) : Text =
            switch (value) {
                case (#placed) "placed";
                case (#approved) "approved";
                case (#delivered) "delivered";
            };
    };
};
