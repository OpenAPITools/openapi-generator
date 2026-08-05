# frozen_string_literal: true

module Qdrant
  module Api
    class Metrics
      def initialize(connection)
        @connection = connection
      end

      def list(anonymize: nil)
        @connection.call(
          :GET,
          '/metrics',
          type: nil,
          auth: ['api-key', 'bearerAuth'],
          query: { 'anonymize' => anonymize }
        )
      end
    end
  end
end
