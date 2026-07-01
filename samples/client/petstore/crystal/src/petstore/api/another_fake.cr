require "json"

module Petstore
  module Api
  class AnotherFake
    def initialize(@conn : Connection); end

    # To test special tags To test special tags and operation ID starting with number
    def dummy(model_client : Petstore::ModelClient) : Response(Petstore::ModelClient)
      @conn.request(Petstore::ModelClient,
        method: :PATCH,
        path: "/another-fake/dummy",
        body: model_client,
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[])
    end
  end
  end

end
