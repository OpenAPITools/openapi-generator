# frozen_string_literal: true

module Qdrant
  module Api
    class Aliases
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/aliases',
          type: Qdrant::Models::GetCollectionAliases200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
