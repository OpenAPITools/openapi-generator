# HTTP::Client::DigestAuth

[![Build Status](https://travis-ci.org/mamantoha/http-client-digest_auth.svg?branch=master)](https://travis-ci.org/mamantoha/http-client-digest_auth)
[![GitHub release](https://img.shields.io/github/release/mamantoha/http-client-digest_auth.svg)](https://github.com/mamantoha/http-client-digest_auth/releases)
[![License](https://img.shields.io/github/license/mamantoha/http-client-digest_auth.svg)](https://github.com/mamantoha/http-client-digest_auth/blob/master/LICENSE)

An implementation of RFC 2617 - Digest Access Authentication. At this time
this library does not drop in to `HTTP::Client` and can be used for with other HTTP
clients.

[crest](https://github.com/mamantoha/crest) uses this shard to provide _Digest Access Authentication_ support out of the box.

In order to use `http-client-digest_auth` you'll need to perform some request
wrangling on your own. See the class documentation at `HTTP::Client::DigestAuth`
for an example.

Ported from Ruby's [net-http-digest_auth](https://github.com/drbrain/net-http-digest_auth) gem.

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  http-client-digest_auth:
    github: mamantoha/http-client-digest_auth
```

## Usage

```crystal
require "http/client"
require "uri"
require "http-client-digest_auth"

url = "https://httpbin.org/digest-auth/auth/admin/passwd/MD5"

uri = URI.parse(url)
uri.user = "admin"
uri.password = "passwd"

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
```

## Contributing

1. Fork it (<https://github.com/mamantoha/http-client-digest_auth/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [Anton Maminov](https://github.com/mamantoha) - creator and maintainer
