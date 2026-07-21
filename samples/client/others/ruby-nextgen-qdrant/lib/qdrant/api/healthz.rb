# frozen_string_literal: true

module Qdrant
  module Api
    class Healthz
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/healthz',
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
