# frozen_string_literal: true

module Qdrant
  module Api
    class Locks
      def initialize(connection)
        @connection = connection
      end

      def create(locks_option: nil)
        @connection.call(
          :POST,
          '/locks',
          type: Qdrant::Models::GetLocks200Response,
          auth: ['api-key', 'bearerAuth'],
          body: locks_option
        )
      end

      def list
        @connection.call(
          :GET,
          '/locks',
          type: Qdrant::Models::GetLocks200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
