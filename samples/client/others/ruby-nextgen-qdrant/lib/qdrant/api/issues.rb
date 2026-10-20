# frozen_string_literal: true

module Qdrant
  module Api
    class Issues
      def initialize(connection)
        @connection = connection
      end

      def bulk_destroy
        @connection.call(
          :DELETE,
          '/issues',
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def list
        @connection.call(
          :GET,
          '/issues',
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
