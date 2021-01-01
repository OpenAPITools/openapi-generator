module Crest
  # Class to convert `Crest::Request` object to cURL command
  #
  # ```crystal
  # request = Crest::Request.new(:post, "http://httpbin.org/post", form: {"title" => "New Title"})
  # Crest::Curlify.to_curl(request)
  # => "curl -X POST http://httpbin.org/post -d 'title=New+Title' -H 'Content-Type: application/x-www-form-urlencoded'"
  # ```
  class Curlify
    # Returns string with cURL command by provided `Crest::Request` object
    def self.to_curl(request : Crest::Request)
      new(request).to_curl
    end

    def initialize(@request : Crest::Request)
    end

    def to_curl
      ["curl", method, url, proxy, basic_auth, form_data, headers].reject(&.empty?).join(" ")
    end

    private def method
      "-X #{@request.method}"
    end

    private def url
      "#{@request.url}"
    end

    private def headers : String
      headers = [] of String
      @request.headers.each do |k, v|
        next if k == "Authorization" && basic_auth? && includes_authorization_header?

        value = v.is_a?(Array) ? v.first.split(";").first : v
        headers << "-H '#{k}: #{value}'"
      end

      headers.join(" ")
    end

    private def form_data : String
      form_data = [] of String

      HTTP::FormData.parse(@request.http_request) do |part|
        value = part.filename ? "@#{part.filename}" : part.body.gets_to_end

        form_data << "-F '#{part.name}=#{value}'"
      end

      form_data.join(" ")
    rescue HTTP::FormData::Error
      body = @request.http_request.body.to_s

      body.empty? ? "" : "-d '#{body}'"
    end

    private def basic_auth : String
      return "" unless basic_auth?

      params = [] of String

      params << "--digest" if @request.auth == "digest"
      params << "--user #{@request.user}:#{@request.password}"

      params.join(" ")
    end

    private def proxy : String
      return "" unless @request.proxy

      params = [] of String

      params << "--proxy #{@request.p_addr}:#{@request.p_port}"
      params << "--proxy-user #{@request.p_user}:#{@request.p_pass}" if proxy_auth?

      params.join(" ")
    end

    private def basic_auth? : Bool
      @request.user && @request.password ? true : false
    end

    private def proxy_auth? : Bool
      @request.p_user && @request.p_pass ? true : false
    end

    private def includes_authorization_header?
      @request.headers.includes_word?("Authorization", "Basic") ||
        @request.headers.includes_word?("Authorization", "Digest")
    end
  end
end
