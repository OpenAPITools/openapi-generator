/// A User who is purchasing from the pet store
import { Candid } "mo:serde-core";
import Array "mo:core/Array";
import List "mo:core/List";
import Float "mo:core/Float";
import Runtime "mo:core/Runtime";

// User.mo

module {
    /// The required-fields slice of User — what `init` consumes.
    /// Exposed so callers can write `let req : Required = {...}` if they want
    /// to manipulate the required-only payload independently of the full record.
    public type Required = {
    };

    // Optional-fields slice. Private — not part of the consumer surface;
    // it's an internal scaffold so we can express User as an
    // `and`-intersection and keep `init` from listing every optional explicitly.
    type Optional = {
        id : ?Int;
        username : ?Text;
        firstName : ?Text;
        lastName : ?Text;
        email : ?Text;
        password : ?Text;
        phone : ?Text;
        userStatus : ?Int;
    };

    public type User = Required and Optional;

    public module JSON {
        // `init` constructs a User from just its required fields,
        // defaulting all optional fields to `null`. Pair with record-update
        // syntax to layer in selected optionals:
        //   let req = { User.init { …required fields… } with someOpt = ?… };
        // Implementation uses Candid round-trip — Candid record subtyping fills
        // absent optional fields with null. Costs a few cycles per call (init is
        // not on a hot path) but keeps generated code compact regardless of how
        // many optional fields the model has.
        public func init(required : Required) : User {
            let ?res = from_candid(to_candid(required)) : ?User else Runtime.unreachable();
            res
        };

        public func toCandidValue(value : User) : Candid.Candid {
            let buf = List.empty<(Text, Candid.Candid)>();
            switch (value.id) {
                case (?v__) List.add(buf, ("id", #Int(v__)));
                case null ();
            };
            switch (value.username) {
                case (?v__) List.add(buf, ("username", #Text(v__)));
                case null ();
            };
            switch (value.firstName) {
                case (?v__) List.add(buf, ("firstName", #Text(v__)));
                case null ();
            };
            switch (value.lastName) {
                case (?v__) List.add(buf, ("lastName", #Text(v__)));
                case null ();
            };
            switch (value.email) {
                case (?v__) List.add(buf, ("email", #Text(v__)));
                case null ();
            };
            switch (value.password) {
                case (?v__) List.add(buf, ("password", #Text(v__)));
                case null ();
            };
            switch (value.phone) {
                case (?v__) List.add(buf, ("phone", #Text(v__)));
                case null ();
            };
            switch (value.userStatus) {
                case (?v__) List.add(buf, ("userStatus", #Int(v__)));
                case null ();
            };
            #Record(List.toArray(buf));
        };

        public func fromCandidValue(candid : Candid.Candid) : ?User =
            switch (candid) {
                case (#Record(fields)) {
                    let id : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "id")) {
                        case (?id_field) ((switch (id_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    let username : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "username")) {
                        case (?username_field) ((switch (username_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let firstName : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "firstName")) {
                        case (?firstName_field) ((switch (firstName_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let lastName : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "lastName")) {
                        case (?lastName_field) ((switch (lastName_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let email : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "email")) {
                        case (?email_field) ((switch (email_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let password : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "password")) {
                        case (?password_field) ((switch (password_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let phone : ?Text = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "phone")) {
                        case (?phone_field) ((switch (phone_field.1) { case (#Text(s)) ?s; case _ null }));
                        case null null;
                    };
                    let userStatus : ?Int = switch (Array.find<(Text, Candid.Candid)>(fields, func((k, _) : (Text, Candid.Candid)) : Bool = k == "userStatus")) {
                        case (?userStatus_field) ((switch (userStatus_field.1) { case (#Int(i)) ?i; case (#Nat(n)) ?n; case _ null }));
                        case null null;
                    };
                    ?{
                        id;
                        username;
                        firstName;
                        lastName;
                        email;
                        password;
                        phone;
                        userStatus;
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
