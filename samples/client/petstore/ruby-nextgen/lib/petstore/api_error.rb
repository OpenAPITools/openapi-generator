# frozen_string_literal: true

module Petstore
  class ApiError < StandardError
    attr_reader :status, :headers, :body, :response

    def initialize(message = nil, status: nil, headers: nil, body: nil, response: nil)
      @status = status
      @headers = headers
      @body = body
      @response = response
      super(message || "HTTP #{status}")
    end

    def self.from(response)
      new(status: response.status, headers: response.headers, body: response.body, response: response)
    end
  end
end
