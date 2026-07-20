require "json"

module Petstore
  module Api
  class Foo
    def initialize(@conn : Connection); end

    # 
    def get() : Response(Petstore::FooGetDefaultResponse)
      @conn.request(Petstore::FooGetDefaultResponse,
        method: :GET,
        path: "/foo",
        accept: %w[application/json],
        auth: %w[])
    end
  end
  end

end
