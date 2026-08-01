# frozen_string_literal: true

module Qdrant
  # Result of an operation. Delegates to `data` for ergonomic access while keeping
  # status/headers available. Replaces the `_with_http_info` twin methods.
  class Response
    extend Forwardable

    attr_reader :data, :status, :headers

    def initialize(data:, status:, headers:)
      @data = data
      @status = status
      @headers = headers
    end

    def_delegators :data, :[], :each, :map, :to_a, :to_hash, :dig

    def to_h
      data.respond_to?(:to_hash) ? data.to_hash : data
    end

    def deconstruct_keys(_keys)
      { data: data, status: status, headers: headers }
    end

    def deconstruct
      [data, status, headers]
    end
  end
end
