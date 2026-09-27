require "json"

module Petstore
  module Api
  class Pet
    def initialize(@conn : Connection); end

    # Update an existing pet 
    def bulk_update(pet : Petstore::Pet) : Response(Nil)
      @conn.request(Nil,
        method: :PUT,
        path: "/pet",
        body: pet,
        accept: %w[],
        content_type: %w[application/json application/xml],
        auth: %w[petstore_auth])
    end

    # Add a new pet to the store 
    def create(pet : Petstore::Pet) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/pet",
        body: pet,
        accept: %w[],
        content_type: %w[application/json application/xml],
        auth: %w[petstore_auth])
    end

    # Updates a pet in the store with form data 
    def create_post(pet_id : Int64, name : String? = nil, status : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/pet/{petId}".sub("{petId}", Petstore.enc(pet_id)),
        form: Hash(String, Crest::ParamsValue){ "name" => name, "status" => status },
        accept: %w[],
        auth: %w[petstore_auth])
    end

    # Deletes a pet 
    def delete(pet_id : Int64, *, api_key : String? = nil) : Response(Nil)
      @conn.request(Nil,
        method: :DELETE,
        path: "/pet/{petId}".sub("{petId}", Petstore.enc(pet_id)),
        header: { "api_key" => api_key.try &.to_s },
        accept: %w[],
        auth: %w[petstore_auth])
    end

    # Finds Pets by status Multiple status values can be provided with comma separated strings
    def find_by_status(*, status : Array(String)? = nil) : Response(Array(Petstore::Pet))
      @conn.request(Array(Petstore::Pet),
        method: :GET,
        path: "/pet/findByStatus",
        query: { "status" => status.try(&.map(&.to_s).join(",")) },
        accept: %w[application/xml application/json],
        auth: %w[petstore_auth])
    end

    # Finds Pets by tags Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    @[Deprecated("This operation is marked deprecated in the OpenAPI spec.")]
    def find_by_tags(*, tags : Set(String)? = nil) : Response(Set(Petstore::Pet))
      @conn.request(Set(Petstore::Pet),
        method: :GET,
        path: "/pet/findByTags",
        query: { "tags" => tags.try(&.map(&.to_s).join(",")) },
        accept: %w[application/xml application/json],
        auth: %w[petstore_auth])
    end

    # Find pet by ID Returns a single pet
    def get(pet_id : Int64) : Response(Petstore::Pet)
      @conn.request(Petstore::Pet,
        method: :GET,
        path: "/pet/{petId}".sub("{petId}", Petstore.enc(pet_id)),
        accept: %w[application/xml application/json],
        auth: %w[api_key])
    end

    # uploads an image 
    def upload_image(pet_id : Int64, additional_metadata : String? = nil, file : ::File? = nil) : Response(Petstore::ApiResponse)
      @conn.request(Petstore::ApiResponse,
        method: :POST,
        path: "/pet/{petId}/uploadImage".sub("{petId}", Petstore.enc(pet_id)),
        form: Hash(String, Crest::ParamsValue){ "additionalMetadata" => additional_metadata, "file" => file },
        accept: %w[application/json],
        auth: %w[petstore_auth])
    end
  end
  end

end
