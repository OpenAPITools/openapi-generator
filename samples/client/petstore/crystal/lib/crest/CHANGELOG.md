# Changelog

## [...]

## [0.26.1][] (2020-07-07)

* Fixed compatibility with Crystal nightly

## [0.26.0][] (2020-06-18)

* Crystal 0.35.0 required
* Use [http_proxy](https://github.com/mamantoha/http_proxy) shard instead of build-in implementation

## [0.25.1][] (2020-06-02)

* Bug fixes and other improvements

## [0.25.0][] (2020-04-07)

* Crystal 0.34.0 required
* Rewrite `Crest::Logger` class
* Fix redirects when "Location" header is downcased

## [0.24.1][] (2020-03-29)

* Fix `handle_errors` is ignored for redirect errors ([#132](https://github.com/mamantoha/crest/issues/132))

## [0.24.0][] (2020-03-13)

* Add `Crest#ParamsEncoder` module to encode/decode URI querystring
* Replace `Crest::Utils#encode_query_string` `with Crest::ParamsEncoder#encode`
* Allow `Boolean` in params

## [0.23.2][] (2020-01-03)

* Fix an issue with wrong "Content-Type" header

## [0.23.1][] (2019-12-14)

* Add a more descriptive crest user agent

## [0.23.0][] (2019-12-12)

* Add methods `to_s` and `inspect` to `Crest::Response`
* Support Crystal 0.32.0

## [0.22.0][] (2019-09-17)

* Support Crystal 0.31.0
* Digest access authentication support ([#127](https://github.com/mamantoha/crest/pull/127))
* Add proxy to `to_curl` method

## [0.21.1][] (2019-08-13)

* **(breaking-change)** Require Crystal 0.30.1

## [0.21.0][] (2019-08-02)

* **(breaking-change)** Require Crystal 0.30.0
* **(breaking-change)** Rename `Crest::Response#successful?` to `Crest::Response#success?`
* Add method `Crest::Response#status` as `HTTP::Status`

## [0.20.0][] (2019-06-14)

* Tested with Crystal 0.29.0
* Improve testing process ([#120](https://github.com/mamantoha/crest/pull/120))

## [0.19.1][] (2019-05-09)

* Delegate method `to_curl` to `Crest::Response` instance
* Fix an issue in `Resource` when base url ends with `/`

## [0.19.0][] (2019-04-18)

* Add method `head` ([#116](https://github.com/mamantoha/crest/pull/116))
* Tested with Crystal 0.28.0

## [0.18.3][] (2019-02-06)

* Tested with Crystal 0.27.2

## [0.18.2][] (2019-02-03)

* Tested with Crystal 0.27.1

## [0.18.1][] (2019-01-16)

* Fix extracting filename from Content-Disposition header

## [0.18.0][] (2019-01-06)

* **(breaking-change)** Streaming support. `Crest`, `Crest::Request` and `Crest::Resource` verb methods(`get`, `post`, etc.) yields the `Crest::Response` as stream to the block ([#110](https://github.com/mamantoha/crest/pull/110))
* **(breaking-change)** Needs to specify `form`, `headers` and `params` arguments for `Crest::Resource` methods ([#112](https://github.com/mamantoha/crest/pull/112))
* Add `Crest::Response#filename` method ([#111](https://github.com/mamantoha/crest/pull/111))
* Add response helper methods (`successful?`, `redirection?`, etc) ([#107](https://github.com/mamantoha/crest/pull/107))
* Extract redirection logic into `Crest::Redirector` class ([#109](https://github.com/mamantoha/crest/pull/109))

## [0.17.0][] (2018-11-17)

* **(breaking-change)** `Crest` and `Crest::Request` verb methods(`get`, `post`, etc.) yields the `Crest::Response` to the block
* Refactor proxy client

## [0.16.1][] (2018-11-05)

* Update to Kemal 0.25.1

## [0.16.0][] (2018-11-03)

* Tested with Crystal 0.27.0

## [0.15.0][] (2018-10-12)

* SSL/TLS support ([#100](https://github.com/mamantoha/crest/pull/100))
* Tested with Crystal 0.26.1

## [0.14.0][] (2018-08-14)

* Tested with Crystal 0.26.0

## [0.13.0][] (2018-08-13)

* Add `Crest::Request#to_curl` to convert request to cURL command ([#95](https://github.com/mamantoha/crest/pull/95))
* Bug fixes and other improvements

## [0.12.0][] (2018-07-17)

* **(breaking-change)** Rename `Request#payload` to `Request#form`
* Use `application/x-www-form-urlencoded` for forms by default. And `multipart/form-data` when a form includes any `<input type="file">` elements.
* Fix serialize query to string representation as http url-encoded

## [0.11.0][] (2018-07-14)

* Add `Logger#filter` method to filter sensitive information from logs with a regex matcher
* Allow to do request with `suburl` through `Request#http_verb(suburl)` method
* Bug fixes and other improvements

## [0.10.2][] (2018-06-15)

* Tested with Crystal 0.25.0

## [0.10.1][] (2018-05-14)

* Fix `Crest::Utils.flatten_params` method ([#85](https://github.com/mamantoha/crest/pull/85))
* Reduce the false positiveness in code as much as possible ([#83](https://github.com/mamantoha/crest/pull/83), thanks @veelenga)

## [0.10.0][] (2018-04-24)

* Add HTTP verb methods (`get`, `post`, etc) to `Crest::Request`
* `Crest` and `Crest::Request` verb methods(`get`, `post`, etc.) can yields the `Crest::Request` to the block
* `Crest::Request` and `Crest::Resource` initializer can accept block
* Access instance of `HTTP::Client` via `Crest::Request#http_client`
* Access instance of `HTTP::Client` via `Crest::Resource#http_client`
* `Crest::Request` and `Crest::Resource` initializer can accept `HTTP::Client` as `http_client`
* Add method `options` to `HTTP::Resource`

## [0.9.10][] (2018-04-08)

* Add option `:handle_errors` to don't raise exceptions but return the `Response`
* Add custom exceptions for each status code

## [0.9.9][] (2018-04-03)

* Add method `OPTIONS`
* Fix `Crest::Response#headers` method to return response headers

## [0.9.8][] (2018-03-18)

* Tested with Crystal 0.24.2
* Fix Basic Authentication

## [0.9.7][] (2018-03-05)

* Allow `Crest::Resource` to accept default `params` and `headers`
* Allow `Crest::Resource` to accept more parameters(proxy authentication credentials, logging setup)
* Refactor exceptions class
* Setup GitHub Pages branch to host docs

## [0.9.6][] (2018-01-05)

* Proxy on redirects
* Logger in redirects

## [0.9.5][] (2017-12-30)

* Bug fixes and performance improvements

## [0.9.4][] (2017-12-25)

* Tested with Crystal 0.24.1

## [0.9.3][] (2017-12-19)

* Add logging

## 0.9.2 (2017-11-01)

* First release :tada:

[...]: https://github.com/mamantoha/crest/compare/v0.26.1...HEAD
[0.26.1]: https://github.com/mamantoha/crest/compare/v0.26.0...v0.26.1
[0.26.0]: https://github.com/mamantoha/crest/compare/v0.25.1...v0.26.0
[0.25.1]: https://github.com/mamantoha/crest/compare/v0.25.0...v0.25.1
[0.25.0]: https://github.com/mamantoha/crest/compare/v0.24.1...v0.25.0
[0.24.1]: https://github.com/mamantoha/crest/compare/v0.24.0...v0.24.1
[0.24.0]: https://github.com/mamantoha/crest/compare/v0.23.2...v0.24.0
[0.23.2]: https://github.com/mamantoha/crest/compare/v0.23.1...v0.23.2
[0.23.1]: https://github.com/mamantoha/crest/compare/v0.23.0...v0.23.1
[0.23.0]: https://github.com/mamantoha/crest/compare/v0.22.0...v0.23.0
[0.22.0]: https://github.com/mamantoha/crest/compare/v0.21.1...v0.22.0
[0.21.1]: https://github.com/mamantoha/crest/compare/v0.21.0...v0.21.1
[0.21.0]: https://github.com/mamantoha/crest/compare/v0.20.0...v0.21.0
[0.20.0]: https://github.com/mamantoha/crest/compare/v0.19.1...v0.20.0
[0.19.1]: https://github.com/mamantoha/crest/compare/v0.19.0...v0.19.1
[0.19.0]: https://github.com/mamantoha/crest/compare/v0.18.3...v0.19.0
[0.18.3]: https://github.com/mamantoha/crest/compare/v0.18.2...v0.18.3
[0.18.2]: https://github.com/mamantoha/crest/compare/v0.18.1...v0.18.2
[0.18.1]: https://github.com/mamantoha/crest/compare/v0.18.0...v0.18.1
[0.18.0]: https://github.com/mamantoha/crest/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/mamantoha/crest/compare/v0.16.1...v0.17.0
[0.16.1]: https://github.com/mamantoha/crest/compare/v0.16.0...v0.16.1
[0.16.0]: https://github.com/mamantoha/crest/compare/v0.15.0...v0.16.0
[0.15.0]: https://github.com/mamantoha/crest/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/mamantoha/crest/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/mamantoha/crest/compare/v0.12.0...v0.13.0
[0.12.0]: https://github.com/mamantoha/crest/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/mamantoha/crest/compare/v0.10.2...v0.11.0
[0.10.2]: https://github.com/mamantoha/crest/compare/v0.10.1...v0.10.2
[0.10.1]: https://github.com/mamantoha/crest/compare/v0.10.0...v0.10.1
[0.10.0]: https://github.com/mamantoha/crest/compare/v0.9.10...v0.10.0
[0.9.10]: https://github.com/mamantoha/crest/compare/v0.9.9...v0.9.10
[0.9.9]: https://github.com/mamantoha/crest/compare/v0.9.8...v0.9.9
[0.9.8]: https://github.com/mamantoha/crest/compare/v0.9.7...v0.9.8
[0.9.7]: https://github.com/mamantoha/crest/compare/v0.9.6...v0.9.7
[0.9.6]: https://github.com/mamantoha/crest/compare/v0.9.5...v0.9.6
[0.9.5]: https://github.com/mamantoha/crest/compare/v0.9.4...v0.9.5
[0.9.4]: https://github.com/mamantoha/crest/compare/v0.9.3...v0.9.4
[0.9.3]: https://github.com/mamantoha/crest/compare/v0.9.2...v0.9.3
