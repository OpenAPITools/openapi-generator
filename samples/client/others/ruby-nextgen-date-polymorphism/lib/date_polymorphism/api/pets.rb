# frozen_string_literal: true

module DatePolymorphism
  module Api
    class Pets
      def initialize(connection)
        @connection = connection
      end

      def list
        @connection.call(
          :GET,
          '/pets',
          type: [DatePolymorphism::Models::Pet],
          auth: []
        )
      end
    end
  end
end
