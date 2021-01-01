require "http/client"
require "uri"
require "digest"

# An implementation of RFC 2617 Digest Access Authentication.
# http://www.rfc-editor.org/rfc/rfc2617.txt
#
# Here is a sample usage of `DigestAuth` on `HTTP::Client`:
#
# ```
# url = "https://httpbin.org/digest-auth/auth/user/password/MD5"
#
# uri = URI.parse(url)
# uri.user = "user"
# uri.password = "password"
#
# client = HTTP::Client.new(uri)
#
# response = client.get(uri.full_path)
# # response is a 401 response with a WWW-Authenticate header
#
# www_authenticate = response.headers["WWW-Authenticate"]
#
# digest_auth = HTTP::Client::DigestAuth.new
# auth = digest_auth.auth_header(uri, www_authenticate, "GET")
#
# http_headers = HTTP::Headers.new
# http_headers["Authorization"] = auth
#
# # re-issue request with Authorization
# response = client.get(uri.full_path, http_headers)
# ```
class HTTP::Client::DigestAuth
  class Error < Exception; end

  def initialize
    @nonce_count = -1
  end

  # Creates a digest auth header for `uri` from the `www_authenticate` header
  # for HTTP method `method`.
  #
  # The result of this method should be sent along with the HTTP request as
  # the "Authorization" header.
  #
  # IIS servers handle the "qop" parameter of digest authentication
  # differently so you may need to set `iis` to true for such servers.
  def auth_header(uri : URI, www_authenticate : String, method : String, iis = false) : String
    nonce_count = next_nonce

    user = uri.user
    password = uri.password

    header = ""
    params = {} of String => String

    if m = www_authenticate.match(/^(\w+) (.*)/)
      challenge = m[2]

      challenge.scan(/(\w+)="(.*?)"/) do |m|
        params[m[1]] = m[2]
      end

      if m = challenge.match(/algorithm="?(.*?)"?([, ]|$)/)
        params["algorithm"] = m[1]
      else
        params["algorithm"] = "MD5"
      end

      if m = params["algorithm"].match(/(.*?)(-sess)?$/)
        algorithm = choose_digest_algorithm(m[1])
        sess = m[2]?
      end

      algorithm = algorithm.not_nil!

      qop = params["qop"]?
      cnonce = make_cnonce if qop || sess

      a1 =
        if sess
          [
            hexdigest(algorithm, "#{user}:#{params["realm"]}:#{password}"),
            params["nonce"],
            cnonce,
          ].join(":")
        else
          "#{user}:#{params["realm"]}:#{password}"
        end

      ha1 = hexdigest(algorithm, a1)
      ha2 = hexdigest(algorithm, "#{method}:#{uri.full_path}")

      request_digest = [] of String | Nil
      request_digest.push(ha1, params["nonce"])
      request_digest.push(("%08x" % nonce_count), cnonce, qop) if qop
      request_digest << ha2
      request_digest = request_digest.join(":")

      response_digest = hexdigest(algorithm, request_digest)

      header = [
        "Digest username=\"#{user}\"",
        "realm=\"#{params["realm"]}\"",
        "algorithm=#{params["algorithm"]}",
        "uri=\"#{uri.full_path}\"",
        "nonce=\"#{params["nonce"]}\"",
        "response=\"#{response_digest}\"",
      ]

      if qop
        if iis
          header << "qop=\"#{qop}\""
        else
          header << "qop=#{qop}"
        end
        header << "nc=#{"%08x" % nonce_count}"
        header << "cnonce=\"#{cnonce}\""
      else
      end

      if params.has_key?("opaque")
        header << "opaque=\"#{params["opaque"]}\""
      end

      header = header.compact.join(", ")
    else
      raise Error.new("Digest auth method not found or syntax error in auth header: #{www_authenticate}")
    end

    header
  end

  private def next_nonce
    @nonce_count += 1
  end

  private def choose_digest_algorithm(algorithm : String) : String
    case algorithm
    when "MD5"     then "MD5"
    when "SHA1"    then "SHA1"
    when "SHA-256" then "SHA256"
    when "SHA-512" then "SHA512"
    else
      raise Error.new("unknown algorithm \"#{algorithm}\"")
    end
  end

  private def hexdigest(algorithm, data)
    digest = OpenSSL::Digest.new(algorithm)
    digest << data
    {% if compare_versions(Crystal::VERSION, "0.35.0-0") >= 0 %}
      digest.final.hexstring
    {% else %}
      digest.hexdigest
    {% end %}
  end

  # Creates a client nonce value that is used across all requests based on the
  # current time, process id and a random number
  private def make_cnonce
    Digest::MD5.hexdigest([
      Time.utc.to_unix,
      Process.pid,
      Random::Secure.rand(2_i64 ** 32),
    ].join(":"))
  end
end
