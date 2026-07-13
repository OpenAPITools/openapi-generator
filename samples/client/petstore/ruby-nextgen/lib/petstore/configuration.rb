# frozen_string_literal: true

module Petstore
  class Configuration
    attr_accessor :base_url, :timeout, :logger, :debugging, :query_array_encoding, :api_key

    def initialize(base_url: nil, **options)
      @base_url = base_url || 'http://petstore.swagger.io/v2'
      @timeout = 60
      @query_array_encoding = :repeat
      @debugging = false
      @middlewares = []
      options.each do |k, v|
        raise ArgumentError, "unknown configuration option: #{k}" unless respond_to?("#{k}=")

        public_send("#{k}=", v)
      end
      yield self if block_given?
    end

    def configure_faraday(conn)
      # Order matters: :multipart / :url_encoded must run before :json so form calls
      # (Connection#call `form:`) get encoded first -- Connection#call pins the
      # Content-Type to "application/json" up front on non-form calls, so these two
      # middlewares' own (weaker) content-type guards correctly no-op on JSON bodies.
      conn.request :multipart
      conn.request :url_encoded
      conn.request :json
      conn.response :json, content_type: /\bjson$/
      conn.options.timeout = timeout
      conn.response :logger, logger if logger && debugging
      @middlewares.each { |mw, mw_args, mw_block| conn.use(mw, *mw_args, &mw_block) }
      conn.adapter Faraday.default_adapter
    end

    # Applies authentication credentials to a single request's headers and query
    # params. Called by Connection#call on EVERY request (not baked into the
    # persistent Faraday connection at build time) so credential rotation --
    # token refresh, a new api_key -- takes effect on the very next call.
    # `auth_names` are the security schemes the current operation actually declares (empty
    # for `security: []` public endpoints); only those are applied, so credentials never
    # leak to public endpoints nor stack conflicting Authorization headers.
    # rubocop:disable Lint/UnusedMethodArgument -- headers/query/auth_names form the fixed
    # per-request auth interface (Connection#call); a given generated client may exercise
    # only some of them (e.g. no query-based key, or no auth method at all).
    def apply_auth(headers, query, auth_names)
      headers['api_key'] = api_key if api_key && auth_names.include?('api_key')
      nil
    end
    # rubocop:enable Lint/UnusedMethodArgument

    # Register a Faraday middleware to run on every request, after the built-in
    # request middleware (so it sees the serialized body and final URL) and just
    # before the adapter. Use for request signing (OVH, AWS SigV4, ...).
    def use(middleware, *args, &block)
      @middlewares << [middleware, args, block]
      self
    end

    def encode_array(values)
      case query_array_encoding
      when :csv then values.join(',')
      else values
      end
    end
  end
end
