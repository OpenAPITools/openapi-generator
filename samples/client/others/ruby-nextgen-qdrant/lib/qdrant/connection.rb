# frozen_string_literal: true

module Qdrant
  # The single HTTP choke-point. Every operation goes through `call`.
  class Connection
    def initialize(configuration)
      @configuration = configuration
      # Force a trailing slash so a base_url that itself carries a path prefix
      # (e.g. "https://api.ovh.com/1.0") is preserved: request paths are made
      # relative (their leading slash is stripped in #call) and resolved against it.
      base = configuration.base_url
      base += '/' unless base.end_with?('/')
      @faraday = Faraday.new(url: base) do |conn|
        configuration.configure_faraday(conn)
      end
    end

    # `body` (JSON) and `form` (urlencoded/multipart) are mutually exclusive -- an
    # operation has either a request body or form params, never both.
    def call(method, path, type: nil, auth: [], query: {}, headers: {}, body: nil, form: nil)
      # Faraday's `run_request` only accepts lowercase method symbols; api.mustache
      # emits the readable uppercase form (e.g. `:GET`), so normalize it here.
      method = method.to_s.downcase.to_sym
      # Make the path relative so Faraday resolves it against the (slash-terminated)
      # base_url without discarding any base path prefix.
      path = path.sub(%r{\A/+}, '')
      request_headers = merge_headers(headers)
      request_query = query.compact.transform_keys(&:to_s)
      # Only the auth schemes the operation actually declares are applied, so credentials
      # never leak to endpoints with `security: []` nor stack conflicting Authorization
      # headers on operations that accept only one of several schemes.
      @configuration.apply_auth(request_headers, request_query, auth)
      request_body =
        if form
          wrap_form(form)
        else
          # Pin the content type ourselves so the `:multipart`/`:url_encoded` request
          # middlewares (registered ahead of `:json` in Configuration#configure_faraday
          # so form calls can be encoded before `:json` sees them) do not mistake a
          # plain JSON Hash/Array body for a form payload -- both only skip a request
          # once its Content-Type is set to something other than what they handle.
          request_headers['Content-Type'] ||= 'application/json' unless body.nil?
          serialize_body(body)
        end

      response = @faraday.run_request(method, path, request_body, request_headers) do |req|
        req.params.update(encode_query(request_query))
      end
      raise ApiError.from(response) unless response.success?

      Response.new(data: deserialize(response.body, type), status: response.status, headers: response.headers)
    rescue Faraday::Error => e
      # Ruby sets #cause automatically to `e` when raising inside a rescue block.
      raise ApiError, e.message
    end

    private

    def serialize_body(body)
      return nil if body.nil?

      body.respond_to?(:to_body) ? body.to_body : body
    end

    def merge_headers(headers)
      headers.compact.transform_keys(&:to_s)
    end

    def encode_query(query)
      query.compact.each_with_object({}) do |(k, v), out|
        out[k.to_s] = v.is_a?(Array) ? @configuration.encode_array(v) : v
      end
    end

    # Drops nil form values and wraps bare IO-like values (e.g. a plain `File.open(...)`)
    # in a `Faraday::Multipart::FilePart` so the `:multipart` middleware's file
    # detection (which looks for `:content_type`, not `:read`) can see them. Values
    # that are already a FilePart/UploadIO (they respond to `:content_type`) or plain
    # scalars pass through untouched; the `:multipart`/`:url_encoded` middlewares
    # decide for themselves (based on whether any part looks like a file) which
    # encoding to apply.
    def wrap_form(form)
      form.compact.transform_values do |v|
        next v if !v.respond_to?(:read) || v.respond_to?(:content_type)

        Faraday::Multipart::FilePart.new(v, 'application/octet-stream')
      end
    end

    def deserialize(body, type)
      return nil if body.nil? || body == ''

      # A successful response with a non-JSON body (plain text, HTML from a
      # misbehaving proxy, ...) must not escape the single ApiError contract.
      data =
        if body.is_a?(String)
          begin
            JSON.parse(body)
          rescue JSON::ParserError => e
            raise ApiError, "invalid JSON in response body: #{e.message}"
          end
        else
          body
        end
      return data if type.nil?

      if type.respond_to?(:from_hash)
        type.from_hash(data)
      elsif type.respond_to?(:build)
        type.build(data)
      elsif type.is_a?(Array) && type.first.respond_to?(:from_hash)
        data.map { |el| type.first.from_hash(el) }
      else
        data
      end
    end
  end
end
