require "json"

module Petstore
  module Api
  class Store::Order
    def initialize(@conn : Connection); end

    # Place an order for a pet 
    def create(order : Petstore::Order) : Response(Petstore::Order)
      @conn.request(Petstore::Order,
        method: :POST,
        path: "/store/order",
        body: order,
        accept: %w[application/xml application/json],
        content_type: %w[application/json],
        auth: %w[])
    end

    # Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    def delete(order_id : String) : Response(Nil)
      @conn.request(Nil,
        method: :DELETE,
        path: "/store/order/{order_id}".sub("{order_id}", Petstore.enc(order_id)),
        accept: %w[],
        auth: %w[])
    end

    # Find purchase order by ID For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
    def get(order_id : Int64) : Response(Petstore::Order)
      @conn.request(Petstore::Order,
        method: :GET,
        path: "/store/order/{order_id}".sub("{order_id}", Petstore.enc(order_id)),
        accept: %w[application/xml application/json],
        auth: %w[])
    end
  end
  end

end
