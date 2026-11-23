# frozen_string_literal: true

require 'spec_helper'

class StubReplyMiddleware < Faraday::Middleware
  def initialize(app, body, status = 200, content_type = 'application/json')
    super(app)
    @body = body
    @status = status
    @content_type = content_type
  end

  def call(env)
    env.status = @status
    env.body = @body
    env.response_headers = Faraday::Utils::Headers.new('Content-Type' => @content_type)
    env.response = Faraday::Response.new(env)
  end
end

RSpec.describe Petstore::Connection do
  def connection_replying_with(body, status: 200, content_type: 'application/json')
    client = Petstore::Client.new(base_url: 'http://localhost') do |c|
      c.use(StubReplyMiddleware, body, status, content_type)
    end
    client.connection
  end

  it 'deserializes a JSON string body to a String instead of raising ApiError' do
    response = connection_replying_with('"hello"').call(:GET, '/greeting')

    expect(response.data).to eq('hello')
    expect(response.status).to eq(200)
  end

  it 'builds a model from a JSON object body' do
    connection = connection_replying_with({ 'name' => 'Rex' }.to_json)
    response = connection.call(:GET, '/pet/1', type: Petstore::Models::Pet)

    expect(response.data).to be_a(Petstore::Models::Pet)
    expect(response.data.name).to eq('Rex')
  end

  it 'raises ApiError on a body that is not valid JSON' do
    expect do
      connection_replying_with('not json').call(:GET, '/greeting')
    end.to raise_error(Petstore::ApiError, /invalid JSON in response body/)
  end

  it 'returns nil data for an empty or blank body' do
    ['', " \n"].each do |body|
      expect(connection_replying_with(body).call(:GET, '/greeting').data).to be_nil
    end
  end

  it 'exposes the decoded body of a JSON error response on ApiError' do
    connection = connection_replying_with({ 'error' => 'Pet not found' }.to_json, status: 404)

    expect { connection.call(:GET, '/pet/1') }.to raise_error(Petstore::ApiError) do |error|
      expect(error.status).to eq(404)
      expect(error.body).to eq('error' => 'Pet not found')
      expect(error.response.body).to eq('error' => 'Pet not found')
    end
  end

  it 'exposes a nil body for an empty JSON error response' do
    connection = connection_replying_with('', status: 500)

    expect { connection.call(:GET, '/pet/1') }.to raise_error(Petstore::ApiError) do |error|
      expect(error.status).to eq(500)
      expect(error.body).to be_nil
    end
  end

  it 'exposes the raw body of an error response that is not JSON' do
    [['text/html', '<h1>Bad Gateway</h1>'], [nil, '{"error":"no content type"}']].each do |(content_type, body)|
      connection = connection_replying_with(body, status: 502, content_type: content_type)

      expect { connection.call(:GET, '/pet/1') }.to raise_error(Petstore::ApiError) do |error|
        expect(error.status).to eq(502)
        expect(error.body).to eq(body)
      end
    end
  end
end
