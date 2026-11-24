# frozen_string_literal: true

module ObjectQuery
  module Api
    class Widgets
      def initialize(connection)
        @connection = connection
      end

      def list(color: nil, flat_color: nil, shape: nil, flat_shape: nil, deep_color: nil, consistency: nil, level: nil, default_color: nil, wrapped_consistency: nil, its: nil)
        @connection.call(
          :GET,
          '/widgets',
          type: nil,
          auth: ['api_key'],
          query: [ObjectQuery::QueryParams.explode('color', color), ObjectQuery::QueryParams.join('flatColor', flat_color), ObjectQuery::QueryParams.explode('shape', shape), ObjectQuery::QueryParams.join('flatShape', flat_shape), ObjectQuery::QueryParams.deep_object('deepColor', deep_color), { 'consistency' => consistency }.compact, { 'level' => level }.compact, ObjectQuery::QueryParams.explode('defaultColor', default_color), { 'wrappedConsistency' => wrapped_consistency }.compact, { 'it\'s' => its }.compact].reduce({}, :merge)
        )
      end
    end
  end
end
