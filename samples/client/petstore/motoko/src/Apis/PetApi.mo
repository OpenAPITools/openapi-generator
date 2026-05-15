// PetApi.mo

import Text "mo:core/Text";
import Int "mo:core/Int";
import Nat "mo:core/Nat";
import Iter "mo:core/Iter";
import Blob "mo:core/Blob";
import Array "mo:core/Array";
import List "mo:core/List";
import Error "mo:core/Error";
import Base64 "mo:core/Base64";
import { JSON; Candid } "mo:serde-core";
import { type ApiResponse; JSON = ApiResponse } "../Models/ApiResponse";
import { type FindPetsByStatusStatusParameterInner; JSON = FindPetsByStatusStatusParameterInner } "../Models/FindPetsByStatusStatusParameterInner";
import { type Pet; JSON = Pet } "../Models/Pet";
import { type Config } "../Config";

module {
    // Management Canister interface for HTTP outcalls
    // Based on https://github.com/dfinity/interface-spec/blob/master/spec/ic.did
    type http_header = {
        name : Text;
        value : Text;
    };

    type http_method = {
        #get;
        #head;
        #post;
        #put;    // Non-replicated only (is_replicated forced to ?false in generated code)
        #delete; // Non-replicated only (is_replicated forced to ?false in generated code)
    };

    type http_request_args = {
        url : Text;
        max_response_bytes : ?Nat64;
        method : http_method;
        headers : [http_header];
        body : ?Blob;
        transform : ?{
            function : shared query ({ response : http_request_result; context : Blob }) -> async http_request_result;
            context : Blob;
        };
        is_replicated : ?Bool;
    };

    type http_request_result = {
        status : Nat;
        headers : [http_header];
        body : Blob;
    };

    let http_request = (actor "aaaaa-aa" : actor { http_request : (http_request_args) -> async http_request_result }).http_request;


    /// Add a new pet to the store
    ///
    /// 
    public func addPet(config : Config, pet : Pet) : async* Pet {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet";

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #post;
            headers;
            body = do ? {
                let candidValue : Candid.Candid = Pet.toCandidValue(pet);
                let #ok(jsonText) = JSON.fromCandid(candidValue)
                    else throw Error.reject("Failed to serialize body to JSON");
                Text.encodeUtf8(jsonText)
            };
        };

        // Call the management canister's http_request method with cycles
        let response : http_request_result = await (with cycles) http_request(request);

        // Check HTTP status code before parsing
        if (response.status >= 200 and response.status < 300) {
            // Success response (2xx): parse as expected return type
            (switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to decode response body as UTF-8");
            }) |>
            (switch (JSON.toCandid(_)) {
                case (#ok(c__)) c__;
                case (#err(msg)) throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to parse JSON: " # msg);
            }) |>
            (switch (Pet.fromCandidValue(_)) {
                case (?value) value;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to convert response to Pet");
            })
        } else {
            // Error response (4xx, 5xx): parse error models and throw
            let responseText = switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null "";  // Empty body for some errors (e.g., 404)
            };

            // 405: Invalid input (no response body model defined)
            if (response.status == 405) {
                throw Error.reject("HTTP 405: Invalid input");
            };

            // Fallback for status codes not defined in OpenAPI spec
            throw Error.reject("HTTP " # Int.toText(response.status) # ": Unexpected error" #
                (if (responseText != "") { " - " # responseText } else { "" }));
        }
    };

    /// Deletes a pet
    ///
    /// 
    public func deletePet(config : Config, petId : Int, apiKey : Text) : async* () {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet/{petId}"
            |> Text.replace(_, #text "{petId}", debug_show(petId));

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" },
            { name = "api_key"; value = apiKey }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #delete;
            headers;
            body = null;
        };

        // Call the management canister's http_request method with cycles
        ignore await (with cycles) http_request(request);

    };

    /// Finds Pets by status
    ///
    /// Multiple status values can be provided with comma separated strings
    public func findPetsByStatus(config : Config, status : [FindPetsByStatusStatusParameterInner]) : async* [Pet] {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet/findByStatus"
            # "?" # "status=" # debug_show(status);

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #get;
            headers;
            body = null;
        };

        // Call the management canister's http_request method with cycles
        let response : http_request_result = await (with cycles) http_request(request);

        // Check HTTP status code before parsing
        if (response.status >= 200 and response.status < 300) {
            // Success response (2xx): parse as expected return type
            (switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to decode response body as UTF-8");
            }) |>
            (switch (JSON.toCandid(_)) {
                case (#ok(c__)) c__;
                case (#err(msg)) throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to parse JSON: " # msg);
            }) |>
            (switch (_) {
                case (#Array(xs__)) {
                    let buf__ = List.empty<Pet>();
                    for (c__ in xs__.values()) {
                        let ?v__ = Pet.fromCandidValue(c__) else throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to convert array element to Pet");
                        List.add(buf__, v__);
                    };
                    List.toArray(buf__);
                };
                case _ throw Error.reject("HTTP " # Int.toText(response.status) # ": Expected JSON array");
            })
        } else {
            // Error response (4xx, 5xx): parse error models and throw
            let responseText = switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null "";  // Empty body for some errors (e.g., 404)
            };

            // 400: Invalid status value (no response body model defined)
            if (response.status == 400) {
                throw Error.reject("HTTP 400: Invalid status value");
            };

            // Fallback for status codes not defined in OpenAPI spec
            throw Error.reject("HTTP " # Int.toText(response.status) # ": Unexpected error" #
                (if (responseText != "") { " - " # responseText } else { "" }));
        }
    };

    /// Finds Pets by tags
    ///
    /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    public func findPetsByTags(config : Config, tags : [Text]) : async* [Pet] {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet/findByTags"
            # "?" # "tags=" # debug_show(tags);

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #get;
            headers;
            body = null;
        };

        // Call the management canister's http_request method with cycles
        let response : http_request_result = await (with cycles) http_request(request);

        // Check HTTP status code before parsing
        if (response.status >= 200 and response.status < 300) {
            // Success response (2xx): parse as expected return type
            (switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to decode response body as UTF-8");
            }) |>
            (switch (JSON.toCandid(_)) {
                case (#ok(c__)) c__;
                case (#err(msg)) throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to parse JSON: " # msg);
            }) |>
            (switch (_) {
                case (#Array(xs__)) {
                    let buf__ = List.empty<Pet>();
                    for (c__ in xs__.values()) {
                        let ?v__ = Pet.fromCandidValue(c__) else throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to convert array element to Pet");
                        List.add(buf__, v__);
                    };
                    List.toArray(buf__);
                };
                case _ throw Error.reject("HTTP " # Int.toText(response.status) # ": Expected JSON array");
            })
        } else {
            // Error response (4xx, 5xx): parse error models and throw
            let responseText = switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null "";  // Empty body for some errors (e.g., 404)
            };

            // 400: Invalid tag value (no response body model defined)
            if (response.status == 400) {
                throw Error.reject("HTTP 400: Invalid tag value");
            };

            // Fallback for status codes not defined in OpenAPI spec
            throw Error.reject("HTTP " # Int.toText(response.status) # ": Unexpected error" #
                (if (responseText != "") { " - " # responseText } else { "" }));
        }
    };

    /// Find pet by ID
    ///
    /// Returns a single pet
    public func getPetById(config : Config, petId : Int) : async* Pet {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet/{petId}"
            |> Text.replace(_, #text "{petId}", debug_show(petId));

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #get;
            headers;
            body = null;
        };

        // Call the management canister's http_request method with cycles
        let response : http_request_result = await (with cycles) http_request(request);

        // Check HTTP status code before parsing
        if (response.status >= 200 and response.status < 300) {
            // Success response (2xx): parse as expected return type
            (switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to decode response body as UTF-8");
            }) |>
            (switch (JSON.toCandid(_)) {
                case (#ok(c__)) c__;
                case (#err(msg)) throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to parse JSON: " # msg);
            }) |>
            (switch (Pet.fromCandidValue(_)) {
                case (?value) value;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to convert response to Pet");
            })
        } else {
            // Error response (4xx, 5xx): parse error models and throw
            let responseText = switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null "";  // Empty body for some errors (e.g., 404)
            };

            // 400: Invalid ID supplied (no response body model defined)
            if (response.status == 400) {
                throw Error.reject("HTTP 400: Invalid ID supplied");
            };
            // 404: Pet not found (no response body model defined)
            if (response.status == 404) {
                throw Error.reject("HTTP 404: Pet not found");
            };

            // Fallback for status codes not defined in OpenAPI spec
            throw Error.reject("HTTP " # Int.toText(response.status) # ": Unexpected error" #
                (if (responseText != "") { " - " # responseText } else { "" }));
        }
    };

    /// Update an existing pet
    ///
    /// 
    public func updatePet(config : Config, pet : Pet) : async* Pet {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet";

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #put;
            headers;
            body = do ? {
                let candidValue : Candid.Candid = Pet.toCandidValue(pet);
                let #ok(jsonText) = JSON.fromCandid(candidValue)
                    else throw Error.reject("Failed to serialize body to JSON");
                Text.encodeUtf8(jsonText)
            };
        };

        // Call the management canister's http_request method with cycles
        let response : http_request_result = await (with cycles) http_request(request);

        // Check HTTP status code before parsing
        if (response.status >= 200 and response.status < 300) {
            // Success response (2xx): parse as expected return type
            (switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to decode response body as UTF-8");
            }) |>
            (switch (JSON.toCandid(_)) {
                case (#ok(c__)) c__;
                case (#err(msg)) throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to parse JSON: " # msg);
            }) |>
            (switch (Pet.fromCandidValue(_)) {
                case (?value) value;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to convert response to Pet");
            })
        } else {
            // Error response (4xx, 5xx): parse error models and throw
            let responseText = switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null "";  // Empty body for some errors (e.g., 404)
            };

            // 400: Invalid ID supplied (no response body model defined)
            if (response.status == 400) {
                throw Error.reject("HTTP 400: Invalid ID supplied");
            };
            // 404: Pet not found (no response body model defined)
            if (response.status == 404) {
                throw Error.reject("HTTP 404: Pet not found");
            };
            // 405: Validation exception (no response body model defined)
            if (response.status == 405) {
                throw Error.reject("HTTP 405: Validation exception");
            };

            // Fallback for status codes not defined in OpenAPI spec
            throw Error.reject("HTTP " # Int.toText(response.status) # ": Unexpected error" #
                (if (responseText != "") { " - " # responseText } else { "" }));
        }
    };

    /// Updates a pet in the store with form data
    ///
    /// 
    public func updatePetWithForm(config : Config, petId : Int, name : Text, status : Text) : async* () {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet/{petId}"
            |> Text.replace(_, #text "{petId}", debug_show(petId));

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #post;
            headers;
            body = null;
        };

        // Call the management canister's http_request method with cycles
        ignore await (with cycles) http_request(request);

    };

    /// uploads an image
    ///
    /// 
    public func uploadFile(config : Config, petId : Int, additionalMetadata : Text, file : Blob) : async* ApiResponse {
        let {baseUrl; cycles} = config;
        let baseUrl__ = baseUrl # "/pet/{petId}/uploadImage"
            |> Text.replace(_, #text "{petId}", debug_show(petId));

        // Add API key as query parameter if using apiKey auth
        let url = switch (config.auth) {
            case _ baseUrl__;
        };

        let baseHeaders = [
            { name = "Content-Type"; value = "application/json; charset=utf-8" }
        ];

        // Build authentication headers based on auth type
        let authHeaders = switch (config.auth) {
            case (?#bearer(token)) {
                [{ name = "Authorization"; value = "Bearer " # token }]
            };
            case (?#apiKey(key)) {
                // API key goes in header
                [{ name = "api_key"; value = key }]
            };
            case (?#basicAuth({user; password})) {
                let encoded = Base64.encode(Text.encodeUtf8(user # ":" # password));
                [{ name = "Authorization"; value = "Basic " # encoded }]
            };
            case null [];
        };

        let headers = Array.flatten<http_header>([
            baseHeaders,
            authHeaders
        ]);

        let request : http_request_args = { config with
            url;
            method = #post;
            headers;
            body = null;
        };

        // Call the management canister's http_request method with cycles
        let response : http_request_result = await (with cycles) http_request(request);

        // Check HTTP status code before parsing
        if (response.status >= 200 and response.status < 300) {
            // Success response (2xx): parse as expected return type
            (switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to decode response body as UTF-8");
            }) |>
            (switch (JSON.toCandid(_)) {
                case (#ok(c__)) c__;
                case (#err(msg)) throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to parse JSON: " # msg);
            }) |>
            (switch (ApiResponse.fromCandidValue(_)) {
                case (?value) value;
                case null throw Error.reject("HTTP " # Int.toText(response.status) # ": Failed to convert response to ApiResponse");
            })
        } else {
            // Error response (4xx, 5xx): parse error models and throw
            let responseText = switch (Text.decodeUtf8(response.body)) {
                case (?text) text;
                case null "";  // Empty body for some errors (e.g., 404)
            };


            // Fallback for status codes not defined in OpenAPI spec
            throw Error.reject("HTTP " # Int.toText(response.status) # ": Unexpected error" #
                (if (responseText != "") { " - " # responseText } else { "" }));
        }
    };


    let operations__ = {
        addPet;
        deletePet;
        findPetsByStatus;
        findPetsByTags;
        getPetById;
        updatePet;
        updatePetWithForm;
        uploadFile;
    };

    public module class PetApi(config : Config) {
        /// Add a new pet to the store
        ///
        /// 
        public func addPet(pet : Pet) : async Pet {
            await* operations__.addPet(config, pet)
        };

        /// Deletes a pet
        ///
        /// 
        public func deletePet(petId : Int, apiKey : Text) : async () {
            await* operations__.deletePet(config, petId, apiKey)
        };

        /// Finds Pets by status
        ///
        /// Multiple status values can be provided with comma separated strings
        public func findPetsByStatus(status : [FindPetsByStatusStatusParameterInner]) : async [Pet] {
            await* operations__.findPetsByStatus(config, status)
        };

        /// Finds Pets by tags
        ///
        /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        public func findPetsByTags(tags : [Text]) : async [Pet] {
            await* operations__.findPetsByTags(config, tags)
        };

        /// Find pet by ID
        ///
        /// Returns a single pet
        public func getPetById(petId : Int) : async Pet {
            await* operations__.getPetById(config, petId)
        };

        /// Update an existing pet
        ///
        /// 
        public func updatePet(pet : Pet) : async Pet {
            await* operations__.updatePet(config, pet)
        };

        /// Updates a pet in the store with form data
        ///
        /// 
        public func updatePetWithForm(petId : Int, name : Text, status : Text) : async () {
            await* operations__.updatePetWithForm(config, petId, name, status)
        };

        /// uploads an image
        ///
        /// 
        public func uploadFile(petId : Int, additionalMetadata : Text, file : Blob) : async ApiResponse {
            await* operations__.uploadFile(config, petId, additionalMetadata, file)
        };

    }
}
