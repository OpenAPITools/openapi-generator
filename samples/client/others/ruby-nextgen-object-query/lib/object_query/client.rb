# frozen_string_literal: true

module ObjectQuery
  class Client
    attr_reader :configuration, :connection

    def initialize(base_url: nil, **options, &block)
      @configuration = Configuration.new(base_url: base_url, **options, &block)
      @connection = Connection.new(@configuration)
    end

    def things
      @things ||= ObjectQuery::Api::Things.new(@connection)
    end

    def widgets
      @widgets ||= ObjectQuery::Api::Widgets.new(@connection)
    end
  end
end
