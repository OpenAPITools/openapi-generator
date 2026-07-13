# frozen_string_literal: true

module Petstore
  module Api
    class Store::Order
      def initialize(connection)
        @connection = connection
      end

      def create(order:)
        raise ArgumentError, 'order is required' if order.nil?

        @connection.call(
          :POST,
          '/store/order',
          type: Petstore::Models::Order,
          auth: [],
          body: order
        )
      end

      def delete(order_id:)
        raise ArgumentError, 'order_id is required' if order_id.nil?

        @connection.call(
          :DELETE,
          '/store/order/{orderId}'
            .gsub('{orderId}', ERB::Util.url_encode(order_id.to_s)),
          type: nil,
          auth: []
        )
      end

      def get(order_id:)
        raise ArgumentError, 'order_id is required' if order_id.nil?

        @connection.call(
          :GET,
          '/store/order/{orderId}'
            .gsub('{orderId}', ERB::Util.url_encode(order_id.to_s)),
          type: Petstore::Models::Order,
          auth: []
        )
      end
    end
  end
end
