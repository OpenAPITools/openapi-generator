require "json"

module Petstore
  module Api
  class FakeClassnameTest
    def initialize(@conn : Connection); end

    # To test class name in snake case To test class name in snake case
    def bulk_partial_update(model_client : Petstore::ModelClient) : Response(Petstore::ModelClient)
      @conn.request(Petstore::ModelClient,
        method: :PATCH,
        path: "/fake_classname_test",
        body: model_client,
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api_key_query])
    end
  end
  end

end
