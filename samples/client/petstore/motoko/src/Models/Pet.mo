/// A pet for sale in the pet store

import { type Category; JSON = Category } "./Category";

import { type PetStatus; JSON = PetStatus } "./PetStatus";

import { type Tag; JSON = Tag } "./Tag";
import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// Pet.mo

module {
    /// The required-fields slice of Pet — what `init` consumes.
    /// Exposed so callers can write `let req : Required = {...}` if they want
    /// to manipulate the required-only payload independently of the full record.
    public type Required = {
        name : Text;
        photoUrls : [Text];
    };

    // Optional-fields slice. Private — not part of the consumer surface;
    // it's an internal scaffold so we can express Pet as an
    // `and`-intersection and keep `init` from listing every optional explicitly.
    type Optional = {
        id : ?Int;
        category : ?Category;
        tags : ?[Tag];
        status : ?PetStatus;
    };

    public type Pet = Required and Optional;

    public module JSON {
        // `init` constructs a Pet from just its required fields,
        // defaulting all optional fields to `null`. Pair with record-update
        // syntax to layer in selected optionals:
        //   let req = { Pet.init { …required fields… } with someOpt = ?… };
        // Implementation uses Candid round-trip — Candid record subtyping fills
        // absent optional fields with null. Costs a few cycles per call (init is
        // not on a hot path) but keeps generated code compact regardless of how
        // many optional fields the model has.
        public func init(required : Required) : Pet {
            let ?res = from_candid(to_candid(required)) : ?Pet else Runtime.unreachable();
            res
        };

        public func toCandidValue(value : Pet) : Candid.Candid {
            let buf = List.empty<(Text, Candid.Candid)>();
            switch (value.id) {
                case (?v__) List.add(buf, ("id", #Int(v__)));
                case null ();
            };
            switch (value.category) {
                case (?v__) List.add(buf, ("category", Category.toCandidValue(v__)));
                case null ();
            };
            List.add(buf, ("name", #Text(value.name)));
            List.add(buf, ("photoUrls", #Array(Array.map<Text, Candid.Candid>(value.photoUrls, func(s : Text) : Candid.Candid = #Text(s)))));
            switch (value.tags) {
                case (?v__) List.add(buf, ("tags", #Array(Array.map<Tag, Candid.Candid>(v__, Tag.toCandidValue))));
                case null ();
            };
            switch (value.status) {
                case (?v__) List.add(buf, ("status", PetStatus.toCandidValue(v__)));
                case null ();
            };
            #Record(List.toArray(buf));
        };

        public func fromCandidValue(candid : Candid.Candid) : ?Pet =
            switch (candid) {
                case (#Record(fields)) {
                    let id : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "id")) {
                        case (?id_field) ((switch (id_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    let category : ?Category = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "category")) {
                        case (?category_field) (Category.fromCandidValue(category_field.1));
                        case null null;
                    };
                    let ?name_field = Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "name") else return null;
                    let ?name = ((switch (name_field.1) { case (#Text(s)) ?s; case _ null })) else return null;
                    let ?photoUrls_field = Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "photoUrls") else return null;
                    let ?photoUrls = ((switch (photoUrls_field.1) {
                        case (#Array(xs__)) {
                            let buf__ = List.empty<Text>();
                            for (c__ in xs__.values()) {
                                let #Text(s__) = c__ else return null;
                                List.add(buf__, s__);
                            };
                            ?List.toArray(buf__);
                        };
                        case _ null;
                    })) else return null;
                    let tags : ?[Tag] = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "tags")) {
                        case (?tags_field) ((switch (tags_field.1) {
                        case (#Array(xs__)) {
                            let buf__ = List.empty<Tag>();
                            for (c__ in xs__.values()) {
                                let ?m__ = Tag.fromCandidValue(c__) else return null;
                                List.add(buf__, m__);
                            };
                            ?List.toArray(buf__);
                        };
                        case _ null;
                    }));
                        case null null;
                    };
                    let status : ?PetStatus = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "status")) {
                        case (?status_field) (PetStatus.fromCandidValue(status_field.1));
                        case null null;
                    };
                    ?{
                        id;
                        category;
                        name;
                        photoUrls;
                        tags;
                        status;
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
