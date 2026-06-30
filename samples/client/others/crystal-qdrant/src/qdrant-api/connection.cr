require "crest"
require "json"
require "log"
require "uri"

module Qdrant::Api
  Log = ::Log.for("qdrant-api")

  def self.enc(value) : String
    URI.encode_path_segment(value.to_s)
  end

  class Connection
    getter config : Configuration

    def initialize(@config : Configuration); end

    def request(klass : T.class, *, method : Symbol, path : String,
                body = nil, query : Hash(String, _)? = nil,
                form : Hash(String, Crest::ParamsValue)? = nil,
                header : Hash(String, String?)? = nil,
                accept : Array(String) = %w[application/json],
                content_type : Array(String) = %w[application/json],
                auth : Array(String) = %w[],
                raw : Bool = false) : Response(T) forall T
      Log.debug { "#{method} #{path}" } if config.debugging

      headers = config.default_headers.dup
      header.try &.each { |k, v| headers[k] = v unless v.nil? }
      # Prefer a JSON media type when the operation offers one: the body is always
      # JSON-decoded (T.from_json), so requesting application/xml first (as some specs
      # list it) would yield a response we can't parse.
      headers["Accept"] = (accept.find(&.includes?("json")) || accept.first) unless accept.empty?
      q = {} of String => String | Array(String)
      query.try &.each do |k, v|
        next if v.nil?
        case v
        when Array then q[k] = v.map(&.to_s)
        else            q[k] = v.to_s
        end
      end
      config.apply_auth!(headers, q, auth)

      # Determine what to pass as the form/body argument to Crest
      # If there's a JSON body, serialize it and pass as raw string form
      body_str : String? = body.nil? ? nil : body.to_json
      headers["Content-Type"] = content_type.first if body_str && !content_type.empty?

      crest_form : Hash(String, Crest::ParamsValue) | String | Nil =
        if body_str
          body_str
        elsif form && !form.empty?
          form
        else
          nil
        end

      headers_hash = {} of String => String | Array(String)
      headers.each { |k, vs| headers_hash[k] = vs.size == 1 ? vs.first : vs }

      resp = Crest::Request.execute(
        method,
        config.base_url + path,
        crest_form,
        headers: headers_hash,
        params: q,
        params_encoder: config.params_encoder,
        logging: config.logging,
        logger: config.logger,
        handle_errors: false)

      resp_headers = to_http_headers(resp.headers)
      unless 200 <= resp.status_code < 300
        raise ApiError.new(resp.status_code, resp_headers, resp.body)
      end

      # `raw` (set by operations whose response isn't JSON, e.g. text/plain or binary) returns the
      # body untouched; a String return type is otherwise JSON-decoded (unquoted).
      value = {% if T == Nil %} nil {% elsif T == String %} (raw ? resp.body : String.from_json(resp.body)) {% else %} T.from_json(resp.body) {% end %}
      Response(T).new(value, resp.status_code, resp_headers)
    end

    # Crest returns headers as a Hash whose values may be a String or an Array(String);
    # convert to the idiomatic HTTP::Headers used by Response/ApiError.
    private def to_http_headers(raw) : HTTP::Headers
      headers = HTTP::Headers.new
      raw.each do |key, value|
        case value
        when Array then value.each { |v| headers.add(key, v) }
        else            headers[key] = value.to_s
        end
      end
      headers
    end
  end
end
