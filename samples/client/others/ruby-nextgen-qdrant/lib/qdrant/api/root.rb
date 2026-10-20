# frozen_string_literal: true

module Qdrant
  module Api
    class Root
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/',
          type: Qdrant::Models::VersionInfo,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
