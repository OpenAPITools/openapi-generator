require "json"

module Petstore
  module Api
  class Store
    def initialize(@conn : Connection); end

    # Returns pet inventories by status Returns a map of status codes to quantities
    def inventory() : Response(Hash(String, Int32))
      @conn.request(Hash(String, Int32),
        method: :GET,
        path: "/store/inventory",
        accept: %w[application/json],
        auth: %w[api_key])
    end
  end
  end

end
