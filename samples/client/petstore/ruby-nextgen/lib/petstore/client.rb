# frozen_string_literal: true

module Petstore
  class Client
    attr_reader :configuration, :connection

    def initialize(base_url: nil, **options, &block)
      @configuration = Configuration.new(base_url: base_url, **options, &block)
      @connection = Connection.new(@configuration)
    end

    def pet
      @pet ||= Petstore::Api::Pet.new(@connection)
    end

    def store
      @store ||= Petstore::Api::Store.new(@connection)
    end

    def user
      @user ||= Petstore::Api::User.new(@connection)
    end
  end
end
