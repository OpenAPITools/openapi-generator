/// An order for a pets from the pet store

import { type OrderStatus; JSON = OrderStatus } "./OrderStatus";
import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// Order.mo

module {
    /// The required-fields slice of Order — what `init` consumes.
    /// Exposed so callers can write `let req : Required = {...}` if they want
    /// to manipulate the required-only payload independently of the full record.
    public type Required = {
    };

    // Optional-fields slice. Private — not part of the consumer surface;
    // it's an internal scaffold so we can express Order as an
    // `and`-intersection and keep `init` from listing every optional explicitly.
    type Optional = {
        id : ?Int;
        petId : ?Int;
        quantity : ?Int;
        shipDate : ?Text;
        status : ?OrderStatus;
        complete : ?Bool;
    };

    public type Order = Required and Optional;

    public module JSON {
        // `init` constructs a Order from just its required fields,
        // defaulting all optional fields to `null`. Pair with record-update
        // syntax to layer in selected optionals:
        //   let req = { Order.init { …required fields… } with someOpt = ?… };
        // Implementation uses Candid round-trip — Candid record subtyping fills
        // absent optional fields with null. Costs a few cycles per call (init is
        // not on a hot path) but keeps generated code compact regardless of how
        // many optional fields the model has.
        public func init(required : Required) : Order {
            let ?res = from_candid(to_candid(required)) : ?Order else Runtime.unreachable();
            res
        };

        public func toCandidValue(value : Order) : Candid.Candid {
            let buf = List.empty<(Text, Candid.Candid)>();
            switch (value.id) {
                case (?v__) List.add(buf, ("id", #Int(v__)));
                case null ();
            };
            switch (value.petId) {
                case (?v__) List.add(buf, ("petId", #Int(v__)));
                case null ();
            };
            switch (value.quantity) {
                case (?v__) List.add(buf, ("quantity", #Int(v__)));
                case null ();
            };
            switch (value.shipDate) {
                case (?v__) List.add(buf, ("shipDate", #Text(v__)));
                case null ();
            };
            switch (value.status) {
                case (?v__) List.add(buf, ("status", OrderStatus.toCandidValue(v__)));
                case null ();
            };
            switch (value.complete) {
                case (?v__) List.add(buf, ("complete", #Bool(v__)));
                case null ();
            };
            #Record(List.toArray(buf));
        };

        public func fromCandidValue(candid : Candid.Candid) : ?Order =
            switch (candid) {
                case (#Record(fields)) {
                    let id : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "id")) {
                        case (?id_field) ((switch (id_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    let petId : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "petId")) {
                        case (?petId_field) ((switch (petId_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    let quantity : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "quantity")) {
                        case (?quantity_field) ((switch (quantity_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    let shipDate : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "shipDate")) {
                        case (?shipDate_field) ((switch (shipDate_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let status : ?OrderStatus = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "status")) {
                        case (?status_field) (OrderStatus.fromCandidValue(status_field.1));
                        case null null;
                    };
                    let complete : ?Bool = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "complete")) {
                        case (?complete_field) ((switch (complete_field.1) { case (#Bool(b)) ?b; case _ null }));
                        case null null;
                    };
                    ?{
                        id;
                        petId;
                        quantity;
                        shipDate;
                        status;
                        complete;
                    };
                };
                case _ null;
            };
    };

    /// Re-export of `JSON.init` at the outer module level. Three import shapes
    /// all reach the same function:
    ///
    ///   - `import T "...";                                     T.init {…}`     // whole-module
    ///   - `import { type T; JSON = T } "...";                  T.init {…}`     // JSON-alias
    ///   - `import { type T; JSON = T; init = myInit } "...";   myInit {…}`     // explicit rename
    ///
    /// The third form is handy when several models would all be reachable
    /// as `T.init` and you want each bound to a distinct local name.
    public let init = JSON.init;
};
