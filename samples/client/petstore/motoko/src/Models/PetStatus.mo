/// pet status in the store
import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// PetStatus.mo
/// Enum values: #available, #pending, #sold

module {
    public type PetStatus = {
        #available;
        #pending;
        #sold;
    };

    public module JSON {
        public func toCandidValue(value : PetStatus) : Candid.Candid =
            switch (value) {
                case (#available) #Text("available");
                case (#pending) #Text("pending");
                case (#sold) #Text("sold");
            };

        public func fromCandidValue(candid : Candid.Candid) : ?PetStatus =
            switch (candid) {
                case (#Text("available")) ?#available;
                case (#Text("pending")) ?#pending;
                case (#Text("sold")) ?#sold;
                case _ null;
            };

        public func toText(value : PetStatus) : Text =
            switch (value) {
                case (#available) "available";
                case (#pending) "pending";
                case (#sold) "sold";
            };
    };
};
