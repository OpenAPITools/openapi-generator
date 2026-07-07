# frozen_string_literal: true

module Qdrant
  module Api
    class Cluster
      def initialize(connection)
        @connection = connection
      end

      def recover
        @connection.call(
          :POST,
          '/cluster/recover',
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def status
        @connection.call(
          :GET,
          '/cluster',
          type: Qdrant::Models::ClusterStatus200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
