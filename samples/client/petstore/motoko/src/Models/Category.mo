/// A category for a pet
import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// Category.mo

module {
    /// The required-fields slice of Category — what `init` consumes.
    /// Exposed so callers can write `let req : Required = {...}` if they want
    /// to manipulate the required-only payload independently of the full record.
    public type Required = {
    };

    // Optional-fields slice. Private — not part of the consumer surface;
    // it's an internal scaffold so we can express Category as an
    // `and`-intersection and keep `init` from listing every optional explicitly.
    type Optional = {
        id : ?Int;
        name : ?Text;
    };

    public type Category = Required and Optional;

    public module JSON {
        // `init` constructs a Category from just its required fields,
        // defaulting all optional fields to `null`. Pair with record-update
        // syntax to layer in selected optionals:
        //   let req = { Category.init { …required fields… } with someOpt = ?… };
        // Implementation uses Candid round-trip — Candid record subtyping fills
        // absent optional fields with null. Costs a few cycles per call (init is
        // not on a hot path) but keeps generated code compact regardless of how
        // many optional fields the model has.
        public func init(required : Required) : Category {
            let ?res = from_candid(to_candid(required)) : ?Category else Runtime.unreachable();
            res
        };

        public func toCandidValue(value : Category) : Candid.Candid {
            let buf = List.empty<(Text, Candid.Candid)>();
            switch (value.id) {
                case (?v__) List.add(buf, ("id", #Int(v__)));
                case null ();
            };
            switch (value.name) {
                case (?v__) List.add(buf, ("name", #Text(v__)));
                case null ();
            };
            #Record(List.toArray(buf));
        };

        public func fromCandidValue(candid : Candid.Candid) : ?Category =
            switch (candid) {
                case (#Record(fields)) {
                    let id : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "id")) {
                        case (?id_field) ((switch (id_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    let name : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "name")) {
                        case (?name_field) ((switch (name_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    ?{
                        id;
                        name;
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
