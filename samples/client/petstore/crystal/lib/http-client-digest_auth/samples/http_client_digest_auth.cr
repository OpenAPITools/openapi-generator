require "http/client"
require "uri"
require "../src/http-client-digest_auth"

path = "auth"     # auth or auth-int
algorithm = "MD5" # MD5, SHA-256, SHA-512
user = "user"
password = "pass"
url = "https://httpbin.org/digest-auth/#{path}/#{user}/#{password}/#{algorithm}"

uri = URI.parse(url)
uri.user = user
uri.password = password

client = HTTP::Client.new(uri)

response = client.get(uri.full_path)
# response is a 401 response with a WWW-Authenticate header

www_authenticate = response.headers["WWW-Authenticate"]

digest_auth = HTTP::Client::DigestAuth.new
auth = digest_auth.auth_header(uri, www_authenticate, "GET")

http_headers = HTTP::Headers.new
http_headers["Authorization"] = auth

# re-issue request with Authorization
response = client.get(uri.full_path, http_headers)

puts response.status_code
puts response.body
