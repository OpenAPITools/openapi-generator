# frozen_string_literal: true

module Qdrant
  module Api
    class Livez
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/livez',
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
