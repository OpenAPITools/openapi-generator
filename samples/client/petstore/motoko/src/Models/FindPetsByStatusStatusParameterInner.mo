import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// FindPetsByStatusStatusParameterInner.mo
/// Enum values: #available, #pending, #sold

module {
    public type FindPetsByStatusStatusParameterInner = {
        #available;
        #pending;
        #sold;
    };

    public module JSON {
        public func toCandidValue(value : FindPetsByStatusStatusParameterInner) : Candid.Candid =
            switch (value) {
                case (#available) #Text("available");
                case (#pending) #Text("pending");
                case (#sold) #Text("sold");
            };

        public func fromCandidValue(candid : Candid.Candid) : ?FindPetsByStatusStatusParameterInner =
            switch (candid) {
                case (#Text("available")) ?#available;
                case (#Text("pending")) ?#pending;
                case (#Text("sold")) ?#sold;
                case _ null;
            };

        public func toText(value : FindPetsByStatusStatusParameterInner) : Text =
            switch (value) {
                case (#available) "available";
                case (#pending) "pending";
                case (#sold) "sold";
            };
    };
};
