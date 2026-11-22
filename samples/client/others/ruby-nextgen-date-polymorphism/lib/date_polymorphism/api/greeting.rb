# frozen_string_literal: true

module DatePolymorphism
  module Api
    class Greeting
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/greeting',
          type: nil,
          auth: []
        )
      end
    end
  end
end
