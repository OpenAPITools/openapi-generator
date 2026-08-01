# frozen_string_literal: true

module Qdrant
  class Client
    attr_reader :configuration, :connection

    def initialize(base_url: nil, **options, &block)
      @configuration = Configuration.new(base_url: base_url, **options, &block)
      @connection = Connection.new(@configuration)
    end

    def aliases
      @aliases ||= Qdrant::Api::Aliases.new(@connection)
    end

    def cluster
      @cluster ||= Qdrant::Api::Cluster.new(@connection)
    end

    def collections
      @collections ||= Qdrant::Api::Collections.new(@connection)
    end

    def healthz
      @healthz ||= Qdrant::Api::Healthz.new(@connection)
    end

    def issues
      @issues ||= Qdrant::Api::Issues.new(@connection)
    end

    def livez
      @livez ||= Qdrant::Api::Livez.new(@connection)
    end

    def locks
      @locks ||= Qdrant::Api::Locks.new(@connection)
    end

    def metrics
      @metrics ||= Qdrant::Api::Metrics.new(@connection)
    end

    def readyz
      @readyz ||= Qdrant::Api::Readyz.new(@connection)
    end

    def root
      @root ||= Qdrant::Api::Root.new(@connection)
    end

    def snapshots
      @snapshots ||= Qdrant::Api::Snapshots.new(@connection)
    end

    def telemetry
      @telemetry ||= Qdrant::Api::Telemetry.new(@connection)
    end
  end
end
