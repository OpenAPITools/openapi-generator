# frozen_string_literal: true

require 'spec_helper'

class StubReplyMiddleware < Faraday::Middleware
  def initialize(app, body)
    super(app)
    @body = body
  end

  def call(env)
    env.status = 200
    env.body = @body
    env.response_headers = Faraday::Utils::Headers.new('Content-Type' => 'application/json')
    env.response = Faraday::Response.new(env)
  end
end

RSpec.describe Petstore::Connection do
  def connection_replying_with(body)
    Petstore::Client.new(base_url: 'http://localhost') { |c| c.use(StubReplyMiddleware, body) }.connection
  end

  it 'deserializes a JSON string body to a String instead of raising ApiError' do
    response = connection_replying_with('"hello"').call(:GET, '/greeting')

    expect(response.data).to eq('hello')
    expect(response.status).to eq(200)
  end

  it 'builds a model from a JSON object body' do
    response = connection_replying_with({ 'name' => 'Rex' }.to_json)
                 .call(:GET, '/pet/1', type: Petstore::Models::Pet)

    expect(response.data).to be_a(Petstore::Models::Pet)
    expect(response.data.name).to eq('Rex')
  end

  it 'raises ApiError on a body that is not valid JSON' do
    expect do
      connection_replying_with('not json').call(:GET, '/greeting')
    end.to raise_error(Petstore::ApiError, /invalid JSON in response body/)
  end

  it 'returns nil data for an empty body' do
    expect(connection_replying_with('').call(:GET, '/greeting').data).to be_nil
  end
end
