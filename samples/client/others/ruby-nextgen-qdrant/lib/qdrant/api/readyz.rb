# frozen_string_literal: true

module Qdrant
  module Api
    class Readyz
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/readyz',
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
