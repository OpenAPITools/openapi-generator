# frozen_string_literal: true

module ObjectQuery
  module Api
    class Things
      def initialize(connection)
        @connection = connection
      end

      def list(filter:, default_filter: nil, flat_filter: nil, deep_filter: nil, limit: nil, tags: nil, content_filter: nil)
        raise ArgumentError, 'filter is required' if filter.nil?

        @connection.call(
          :GET,
          '/things',
          type: nil,
          auth: [],
          query: [ObjectQuery::QueryParams.explode('filter', filter), ObjectQuery::QueryParams.explode('defaultFilter', default_filter), ObjectQuery::QueryParams.join('flatFilter', flat_filter), ObjectQuery::QueryParams.deep_object('deepFilter', deep_filter), { 'limit' => limit }.compact, { 'tags' => tags }.compact, { 'contentFilter' => content_filter }.compact].reduce({}, :merge)
        )
      end
    end
  end
end
