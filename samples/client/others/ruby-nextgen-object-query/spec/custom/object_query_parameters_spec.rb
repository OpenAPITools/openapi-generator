# frozen_string_literal: true

require 'spec_helper'
require 'uri'

# Hand-written (not generated): checks the query string that actually leaves the client
# for object-typed query parameters, per their OpenAPI `style` and `explode`.
RSpec.describe 'object query parameters' do
  # Answers every request with 200 and records the URL it would have gone out with.
  let(:capture) do
    Class.new(Faraday::Middleware) do
      class << self
        attr_accessor :url
      end

      def call(env)
        self.class.url = env.url
        env.status = 200
        env.body = ''
        env.response_headers = Faraday::Utils::Headers.new
        env.response = Faraday::Response.new(env)
      end
    end
  end

  let(:client) do
    middleware = capture
    ObjectQuery::Client.new(base_url: 'http://localhost') { |config| config.use(middleware) }
  end

  # Decoded [name, value] pairs of the query string of the last request.
  def sent_query
    URI.decode_www_form(capture.url.query.to_s)
  end

  describe 'a map (additionalProperties)' do
    # Responds to `to_hash` without honouring it: the conversion is not a Hash.
    let(:not_a_hash) do
      Class.new do
        def to_hash
          'nope'
        end

        def to_s
          'odd'
        end
      end.new
    end

    it 'sends each entry at the top level with style: form, explode: true' do
      client.things.list(filter: { 'tld' => 'com', 'createdDate:gte' => '2023-01-01' })

      expect(sent_query).to contain_exactly(%w[tld com], %w[createdDate:gte 2023-01-01])
    end

    it 'explodes when style and explode are left to their defaults' do
      client.things.list(filter: {}, default_filter: { 'a' => 'b' })

      expect(sent_query).to contain_exactly(%w[a b])
    end

    it 'accepts symbol keys and omits nil entries' do
      client.things.list(filter: { tld: 'com', skip: nil })

      expect(sent_query).to contain_exactly(%w[tld com])
    end

    it 'keeps an entry named like an omitted optional parameter' do
      client.things.list(filter: { 'limit' => '99' })

      expect(sent_query).to contain_exactly(%w[limit 99])
    end

    it 'keeps an entry when a later map holds nil under the same name' do
      client.things.list(filter: { 'tld' => 'com' }, default_filter: { 'tld' => nil })

      expect(sent_query).to contain_exactly(%w[tld com])
    end

    it 'omits nil entries with style: form, explode: false' do
      client.things.list(filter: {}, flat_filter: { 'tld' => 'com', 'skip' => nil })

      expect(sent_query).to contain_exactly(['flatFilter', 'tld,com'])
    end

    it 'sends one comma-separated value with style: form, explode: false' do
      client.things.list(filter: {}, flat_filter: { 'tld' => 'com', 'x' => 'y' })

      expect(sent_query).to contain_exactly(['flatFilter', 'tld,com,x,y'])
    end

    it 'omits an empty map with style: form, explode: false' do
      client.things.list(filter: {}, flat_filter: {})

      expect(sent_query).to be_empty
    end

    it 'brackets the keys with style: deepObject' do
      client.things.list(filter: {}, deep_filter: { 'tld' => 'com' })

      expect(sent_query).to contain_exactly(['deepFilter[tld]', 'com'])
    end

    it 'serializes a model nested in the map with style: deepObject' do
      nested = ObjectQuery::Models::Color.new(red: 1, line_width: 2)
      client.things.list(filter: {}, deep_filter: { 'primary' => nested })

      expect(sent_query).to contain_exactly(['deepFilter[primary][red]', '1'], ['deepFilter[primary][lineWidth]', '2'])
    end

    it 'omits a nil nested in the map with style: deepObject' do
      client.things.list(filter: {}, deep_filter: { 'a' => { 'b' => nil, 'c' => 1 } })

      expect(sent_query).to contain_exactly(['deepFilter[a][c]', '1'])
    end

    it 'sends a nested object whose to_hash is not a Hash as a plain value' do
      client.things.list(filter: {}, deep_filter: { 'a' => not_a_hash })

      expect(sent_query).to contain_exactly(['deepFilter[a]', 'odd'])
    end

    it 'rejects an object whose to_hash is not a Hash' do
      expect { client.things.list(filter: not_a_hash) }
        .to raise_error(ArgumentError, /\Afilter must be a Hash or a model, got #<Class:/)
    end

    it 'rejects a nested object with style: form, explode: false' do
      expect { client.things.list(filter: {}, flat_filter: { 'a' => { 'b' => 1 } }) }
        .to raise_error(ArgumentError, 'flatFilter.a must be a scalar with explode: false, got Hash')
    end

    it 'rejects a nested array with style: form, explode: false' do
      expect { client.things.list(filter: {}, flat_filter: { 'a' => [1, 2] }) }
        .to raise_error(ArgumentError, 'flatFilter.a must be a scalar with explode: false, got Array')
    end

    it 'rejects a value that is neither a Hash nor a model' do
      expect { client.things.list(filter: 'oops') }
        .to raise_error(ArgumentError, 'filter must be a Hash or a model, got String')
    end

    it 'rejects a non-Hash value with style: form, explode: false' do
      expect { client.things.list(filter: {}, flat_filter: 'oops') }
        .to raise_error(ArgumentError, 'flatFilter must be a Hash or a model, got String')
    end
  end

  describe 'a model with declared properties' do
    let(:color) { ObjectQuery::Models::Color.new(red: 1, line_width: 2) }

    it 'sends each property at the top level, under its JSON name, with explode: true' do
      client.widgets.list(color: color)

      expect(sent_query).to contain_exactly(%w[red 1], %w[lineWidth 2])
    end

    it 'explodes when style and explode are left to their defaults' do
      client.widgets.list(default_color: color)

      expect(sent_query).to contain_exactly(%w[red 1], %w[lineWidth 2])
    end

    it 'accepts a plain Hash in place of the model' do
      client.widgets.list(color: { 'red' => 1 })

      expect(sent_query).to contain_exactly(%w[red 1])
    end

    it 'sends one comma-separated value with explode: false' do
      client.widgets.list(flat_color: color)

      expect(sent_query).to contain_exactly(['flatColor', 'red,1,lineWidth,2'])
    end

    it 'brackets the properties with style: deepObject' do
      client.widgets.list(deep_color: color)

      expect(sent_query).to contain_exactly(['deepColor[red]', '1'], ['deepColor[lineWidth]', '2'])
    end
  end

  describe 'a model with declared properties and additionalProperties' do
    let(:shape) { ObjectQuery::Models::Shape.new(kind: 'round', size: 'big') }

    it 'sends declared and additional properties at the top level with explode: true' do
      client.widgets.list(shape: shape)

      expect(sent_query).to contain_exactly(%w[kind round], %w[size big])
    end

    it 'sends one comma-separated value with explode: false' do
      client.widgets.list(flat_shape: shape)

      expect(sent_query).to contain_exactly(['flatShape', 'kind,round,size,big'])
    end
  end

  describe 'a referenced schema that is not an object' do
    it 'sends the integer branch of an anyOf as a plain value' do
      client.widgets.list(consistency: 2)

      expect(sent_query).to contain_exactly(%w[consistency 2])
    end

    it 'sends the enum branch of an anyOf as a plain value' do
      client.widgets.list(consistency: 'majority')

      expect(sent_query).to contain_exactly(%w[consistency majority])
    end

    it 'sends an enum as a plain value' do
      client.widgets.list(level: 'all')

      expect(sent_query).to contain_exactly(%w[level all])
    end

    it 'sends an allOf wrapper around a scalar anyOf as a plain value' do
      client.widgets.list(wrapped_consistency: 2)

      expect(sent_query).to contain_exactly(%w[wrappedConsistency 2])
    end
  end

  describe 'a parameter next to an object parameter' do
    it 'keeps a name holding a quote' do
      client.widgets.list(its: 'x')

      expect(sent_query).to contain_exactly(["it's", 'x'])
    end

    it 'wins over an exploded entry of the same name declared before it' do
      client.things.list(filter: { 'limit' => '99' }, limit: 5)

      expect(sent_query).to contain_exactly(%w[limit 5])
    end

    it 'loses to an API key sent in the query under the same name' do
      middleware = capture
      keyed = ObjectQuery::Client.new(base_url: 'http://localhost', api_key: 'secret') do |config|
        config.use(middleware)
      end
      keyed.widgets.list(shape: { 'api_key' => 'mine' })

      expect(sent_query).to contain_exactly(%w[api_key secret])
    end
  end
end
