# frozen_string_literal: true

module Qdrant
  module Api
    class Telemetry
      def initialize(connection)
        @connection = connection
      end

      def list(anonymize: nil)
        @connection.call(
          :GET,
          '/telemetry',
          type: Qdrant::Models::Telemetry200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'anonymize' => anonymize }
        )
      end
    end
  end
end
