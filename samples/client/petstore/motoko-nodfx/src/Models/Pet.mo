/// A pet for sale in the pet store

import { type Category; JSON = Category } "./Category";

import { type PetStatus; JSON = PetStatus } "./PetStatus";

import { type Tag; JSON = Tag } "./Tag";

// Pet.mo

module {
    // User-facing type: what application code uses
    public type Pet = {
        id : ?Int;
        category : ?Category;
        name : Text;
        photoUrls : [Text];
        tags : ?[Tag];
        status : ?PetStatus;
    };

    // JSON sub-module: everything needed for JSON serialization
    public module JSON {
        // JSON-facing Motoko type: mirrors JSON structure
        // Named "JSON" to avoid shadowing the outer Pet type
        public type JSON = {
            id : ?Int;
            category : ?Category;
            name : Text;
            photoUrls : [Text];
            tags : ?[Tag];
            status : ?PetStatus.JSON;
        };

        // Convert User-facing type to JSON-facing Motoko type
        public func toJSON(value : Pet) : JSON = { value with
            status = do ? { PetStatus.toJSON(value.status!) };
        };

        // Convert JSON-facing Motoko type to User-facing type
        public func fromJSON(json : JSON) : ?Pet {
            ?{ json with
                status = do ? { PetStatus.fromJSON(json.status!)! };
            }
        };
    }
}
