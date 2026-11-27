# frozen_string_literal: true

module DatePolymorphism
  class Client
    attr_reader :configuration, :connection

    def initialize(base_url: nil, **options, &block)
      @configuration = Configuration.new(base_url: base_url, **options, &block)
      @connection = Connection.new(@configuration)
    end

    def greeting
      @greeting ||= DatePolymorphism::Api::Greeting.new(@connection)
    end

    def pets
      @pets ||= DatePolymorphism::Api::Pets.new(@connection)
    end
  end
end
