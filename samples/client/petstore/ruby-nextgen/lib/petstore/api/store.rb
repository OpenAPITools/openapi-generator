# frozen_string_literal: true

module Petstore
  module Api
    class Store
      def initialize(connection)
        @connection = connection
      end

      def inventory
        @connection.call(
          :GET,
          '/store/inventory',
          type: nil,
          auth: ['api_key']
        )
      end

      def order
        @order ||= Store::Order.new(@connection)
      end
    end
  end
end
