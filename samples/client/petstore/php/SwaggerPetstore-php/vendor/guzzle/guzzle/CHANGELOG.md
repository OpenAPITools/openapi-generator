# CHANGELOG

## 3.9.3 - 2015-03-18

* Ensuring Content-Length is not stripped from a request when it is `0`.
* Added more information to stream wrapper exceptions.
* Message parser will no longer throw warnings for malformed messages.
* Giving a valid cache TTL when max-age is 0.

## 3.9.2 - 2014-09-10

* Retrying "Connection died, retrying a fresh connect" curl errors.
* Automatically extracting the cacert from the phar in client constructor.
* Added EntityBody support for OPTIONS requests.

## 3.9.1 - 2014-05-07

* Added a fix to ReadLimitEntityBody to ensure it doesn't infinitely loop.
* Added a fix to the stream checksum function so that when the first read
  returns a falsey value, it still continues to consume the stream until EOF.

## 3.9.0 - 2014-04-23

* `null`, `false`, and `"_guzzle_blank_"` all now serialize as an empty value
  with no trailing "=". See dc1d824277.
* No longer performing an MD5 check on the cacert each time the phar is used,
  but rather copying the cacert to the temp directory.
* `"0"` can now be added as a URL path
* Deleting cookies that are set to empty
* If-Modified-Since is no longer unnecessarily added to the CachePlugin
* Cookie path matching now follows RFC 6265 s5.1.4
* Updated service descriptions are now added to a service client's composite
  factory.
* MockPlugin now throws an exception if the queue is empty.
* Properly parsing URLs that start with "http" but are not absolute
* Added the ability to configure the curl_multi_select timeout setting
* OAuth parameters are now sorted using lexicographical byte value ordering
* Fixing invalid usage of an out of range PHP feature in the ErrorResponsePlugin

## 3.8.1 -2014-01-28

* Bug: Always using GET requests when redirecting from a 303 response
* Bug: CURLOPT_SSL_VERIFYHOST is now correctly set to false when setting `$certificateAuthority` to false in
  `Guzzle\Http\ClientInterface::setSslVerification()`
* Bug: RedirectPlugin now uses strict RFC 3986 compliance when combining a base URL with a relative URL
* Bug: The body of a request can now be set to `"0"`
* Sending PHP stream requests no longer forces `HTTP/1.0`
* Adding more information to ExceptionCollection exceptions so that users have more context, including a stack trace of
  each sub-exception
* Updated the `$ref` attribute in service descriptions to merge over any existing parameters of a schema (rather than
  clobbering everything).
* Merging URLs will now use the query string object from the relative URL (thus allowing custom query aggregators)
* Query strings are now parsed in a way that they do no convert empty keys with no value to have a dangling `=`.
  For example `foo&bar=baz` is now correctly parsed and recognized as `foo&bar=baz` rather than `foo=&bar=baz`.
* Now properly escaping the regular expression delimiter when matching Cookie domains.
* Network access is now disabled when loading XML documents

## 3.8.0 - 2013-12-05

* Added the ability to define a POST name for a file
* JSON response parsing now properly walks additionalProperties
* cURL error code 18 is now retried automatically in the BackoffPlugin
* Fixed a cURL error when URLs contain fragments
* Fixed an issue in the BackoffPlugin retry event where it was trying to access all exceptions as if they were
  CurlExceptions
* CURLOPT_PROGRESS function fix for PHP 5.5 (69fcc1e)
* Added the ability for Guzzle to work with older versions of cURL that do not support `CURLOPT_TIMEOUT_MS`
* Fixed a bug that was encountered when parsing empty header parameters
* UriTemplate now has a `setRegex()` method to match the docs
* The `debug` request parameter now checks if it is truthy rather than if it exists
* Setting the `debug` request parameter to true shows verbose cURL output instead of using the LogPlugin
* Added the ability to combine URLs using strict RFC 3986 compliance
* Command objects can now return the validation errors encountered by the command
* Various fixes to cache revalidation (#437 and 29797e5)
* Various fixes to the AsyncPlugin
* Cleaned up build scripts

## 3.7.4 - 2013-10-02

* Bug fix: 0 is now an allowed value in a description parameter that has a default value (#430)
* Bug fix: SchemaFormatter now returns an integer when formatting to a Unix timestamp
  (see https://github.com/aws/aws-sdk-php/issues/147)
* Bug fix: Cleaned up and fixed URL dot segment removal to properly resolve internal dots
* Minimum PHP version is now properly specified as 5.3.3 (up from 5.3.2) (#420)
* Updated the bundled cacert.pem (#419)
* OauthPlugin now supports adding authentication to headers or query string (#425)

## 3.7.3 - 2013-09-08

* Added the ability to get the exception associated with a request/command when using `MultiTransferException` and
  `CommandTransferException`.
* Setting `additionalParameters` of a response to false is now honored when parsing responses with a service description
* Schemas are only injected into response models when explicitly configured.
* No longer guessing Content-Type based on the path of a request. Content-Type is now only guessed based on the path of
  an EntityBody.
* Bug fix: ChunkedIterator can now properly chunk a \Traversable as well as an \Iterator.
* Bug fix: FilterIterator now relies on `\Iterator` instead of `\Traversable`.
* Bug fix: Gracefully handling malformed responses in RequestMediator::writeResponseBody()
* Bug fix: Replaced call to canCache with canCacheRequest in the CallbackCanCacheStrategy of the CachePlugin
* Bug fix: Visiting XML attributes first before visting XML children when serializing requests
* Bug fix: Properly parsing headers that contain commas contained in quotes
* Bug fix: mimetype guessing based on a filename is now case-insensitive

## 3.7.2 - 2013-08-02

* Bug fix: Properly URL encoding paths when using the PHP-only version of the UriTemplate expander
  See https://github.com/guzzle/guzzle/issues/371
* Bug fix: Cookie domains are now matched correctly according to RFC 6265
  See https://github.com/guzzle/guzzle/issues/377
* Bug fix: GET parameters are now used when calculating an OAuth signature
* Bug fix: Fixed an issue with cache revalidation where the If-None-Match header was being double quoted
* `Guzzle\Common\AbstractHasDispatcher::dispatch()` now returns the event that was dispatched
* `Guzzle\Http\QueryString::factory()` now guesses the most appropriate query aggregator to used based on the input.
  See https://github.com/guzzle/guzzle/issues/379
* Added a way to add custom domain objects to service description parsing using the `operation.parse_class` event. See
  https://github.com/guzzle/guzzle/pull/380
* cURL multi cleanup and optimizations

## 3.7.1 - 2013-07-05

* Bug fix: Setting default options on a client now works
* Bug fix: Setting options on HEAD requests now works. See #352
* Bug fix: Moving stream factory before send event to before building the stream. See #353
* Bug fix: Cookies no longer match on IP addresses per RFC 6265
* Bug fix: Correctly parsing header parameters that are in `<>` and quotes
* Added `cert` and `ssl_key` as request options
* `Host` header can now diverge from the host part of a URL if the header is set manually
* `Guzzle\Service\Command\LocationVisitor\Request\XmlVisitor` was rewritten to change from using SimpleXML to XMLWriter
* OAuth parameters are only added via the plugin if they aren't already set
* Exceptions are now thrown when a URL cannot be parsed
* Returning `false` if `Guzzle\Http\EntityBody::getContentMd5()` fails
* Not setting a `Content-MD5` on a command if calculating the Content-MD5 fails via the CommandContentMd5Plugin

## 3.7.0 - 2013-06-10

* See UPGRADING.md for more information on how to upgrade.
* Requests now support the ability to specify an array of $options when creating a request to more easily modify a
  request. You can pass a 'request.options' configuration setting to a client to apply default request options to
  every request created by a client (e.g. default query string variables, headers, curl options, etc).
* Added a static facade class that allows you to use Guzzle with static methods and mount the class to `\Guzzle`.
  See `Guzzle\Http\StaticClient::mount`.
* Added `command.request_options` to `Guzzle\Service\Command\AbstractCommand` to pass request options to requests
      created by a command (e.g. custom headers, query string variables, timeout settings, etc).
* Stream size in `Guzzle\Stream\PhpStreamRequestFactory` will now be set if Content-Length is returned in the
  headers of a response
* Added `Guzzle\Common\Collection::setPath($path, $value)` to set a value into an array using a nested key
  (e.g. `$collection->setPath('foo/baz/bar', 'test'); echo $collection['foo']['bar']['bar'];`)
* ServiceBuilders now support storing and retrieving arbitrary data
* CachePlugin can now purge all resources for a given URI
* CachePlugin can automatically purge matching cached items when a non-idempotent request is sent to a resource
* CachePlugin now uses the Vary header to determine if a resource is a cache hit
* `Guzzle\Http\Message\Response` now implements `\Serializable`
* Added `Guzzle\Cache\CacheAdapterFactory::fromCache()` to more easily create cache adapters
* `Guzzle\Service\ClientInterface::execute()` now accepts an array, single command, or Traversable
* Fixed a bug in `Guzzle\Http\Message\Header\Link::addLink()`
* Better handling of calculating the size of a stream in `Guzzle\Stream\Stream` using fstat() and caching the size
* `Guzzle\Common\Exception\ExceptionCollection` now creates a more readable exception message
* Fixing BC break: Added back the MonologLogAdapter implementation rather than extending from PsrLog so that older
  Symfony users can still use the old version of Monolog.
* Fixing BC break: Added the implementation back in for `Guzzle\Http\Message\AbstractMessage::getTokenizedHeader()`.
  Now triggering an E_USER_DEPRECATED warning when used. Use `$message->getHeader()->parseParams()`.
* Several performance improvements to `Guzzle\Common\Collection`
* Added an `$options` argument to the end of the following methods of `Guzzle\Http\ClientInterface`:
  createRequest, head, delete, put, patch, post, options, prepareRequest
* Added an `$options` argument to the end of `Guzzle\Http\Message\Request\RequestFactoryInterface::createRequest()`
* Added an `applyOptions()` method to `Guzzle\Http\Message\Request\RequestFactoryInterface`
* Changed `Guzzle\Http\ClientInterface::get($uri = null, $headers = null, $body = null)` to
  `Guzzle\Http\ClientInterface::get($uri = null, $headers = null, $options = array())`. You can still pass in a
  resource, string, or EntityBody into the $options parameter to specify the download location of the response.
* Changed `Guzzle\Common\Collection::__construct($data)` to no longer accepts a null value for `$data` but a
  default `array()`
* Added `Guzzle\Stream\StreamInterface::isRepeatable`
* Removed `Guzzle\Http\ClientInterface::setDefaultHeaders(). Use
  $client->getConfig()->setPath('request.options/headers/{header_name}', 'value')`. or
  $client->getConfig()->setPath('request.options/headers', array('header_name' => 'value'))`.
* Removed `Guzzle\Http\ClientInterface::getDefaultHeaders(). Use $client->getConfig()->getPath('request.options/headers')`.
* Removed `Guzzle\Http\ClientInterface::expandTemplate()`
* Removed `Guzzle\Http\ClientInterface::setRequestFactory()`
* Removed `Guzzle\Http\ClientInterface::getCurlMulti()`
* Removed `Guzzle\Http\Message\RequestInterface::canCache`
* Removed `Guzzle\Http\Message\RequestInterface::setIsRedirect`
* Removed `Guzzle\Http\Message\RequestInterface::isRedirect`
* Made `Guzzle\Http\Client::expandTemplate` and `getUriTemplate` protected methods.
* You can now enable E_USER_DEPRECATED warnings to see if you are using a deprecated method by setting
  `Guzzle\Common\Version::$emitWarnings` to true.
* Marked `Guzzle\Http\Message\Request::isResponseBodyRepeatable()` as deprecated. Use
      `$request->getResponseBody()->isRepeatable()` instead.
* Marked `Guzzle\Http\Message\Request::canCache()` as deprecated. Use
  `Guzzle\Plugin\Cache\DefaultCanCacheStrategy->canCacheRequest()` instead.
* Marked `Guzzle\Http\Message\Request::canCache()` as deprecated. Use
  `Guzzle\Plugin\Cache\DefaultCanCacheStrategy->canCacheRequest()` instead.
* Marked `Guzzle\Http\Message\Request::setIsRedirect()` as deprecated. Use the HistoryPlugin instead.
* Marked `Guzzle\Http\Message\Request::isRedirect()` as deprecated. Use the HistoryPlugin instead.
* Marked `Guzzle\Cache\CacheAdapterFactory::factory()` as deprecated
* Marked 'command.headers', 'command.response_body' and 'command.on_complete' as deprecated for AbstractCommand.
  These will work through Guzzle 4.0
* Marked 'request.params' for `Guzzle\Http\Client` as deprecated. Use [request.options][params].
* Marked `Guzzle\Service\Client::enableMagicMethods()` as deprecated. Magic methods can no longer be disabled on a Guzzle\Service\Client.
* Marked `Guzzle\Service\Client::getDefaultHeaders()` as deprecated. Use $client->getConfig()->getPath('request.options/headers')`.
* Marked `Guzzle\Service\Client::setDefaultHeaders()` as deprecated. Use $client->getConfig()->setPath('request.options/headers/{header_name}', 'value')`.
* Marked `Guzzle\Parser\Url\UrlParser` as deprecated. Just use PHP's `parse_url()` and percent encode your UTF-8.
* Marked `Guzzle\Common\Collection::inject()` as deprecated.
* Marked `Guzzle\Plugin\CurlAuth\CurlAuthPlugin` as deprecated. Use `$client->getConfig()->setPath('request.options/auth', array('user', 'pass', 'Basic|Digest');`
* CacheKeyProviderInterface and DefaultCacheKeyProvider are no longer used. All of this logic is handled in a
  CacheStorageInterface. These two objects and interface will be removed in a future version.
* Always setting X-cache headers on cached responses
* Default cache TTLs are now handled by the CacheStorageInterface of a CachePlugin
* `CacheStorageInterface::cache($key, Response $response, $ttl = null)` has changed to `cache(RequestInterface
  $request, Response $response);`
* `CacheStorageInterface::fetch($key)` has changed to `fetch(RequestInterface $request);`
* `CacheStorageInterface::delete($key)` has changed to `delete(RequestInterface $request);`
* Added `CacheStorageInterface::purge($url)`
* `DefaultRevalidation::__construct(CacheKeyProviderInterface $cacheKey, CacheStorageInterface $cache, CachePlugin
  $plugin)` has changed to `DefaultRevalidation::__construct(CacheStorageInterface $cache,
  CanCacheStrategyInterface $canCache = null)`
* Added `RevalidationInterface::shouldRevalidate(RequestInterface $request, Response $response)`

## 3.6.0 - 2013-05-29

* ServiceDescription now implements ToArrayInterface
* Added command.hidden_params to blacklist certain headers from being treated as additionalParameters
* Guzzle can now correctly parse incomplete URLs
* Mixed casing of headers are now forced to be a single consistent casing across all values for that header.
* Messages internally use a HeaderCollection object to delegate handling case-insensitive header resolution
* Removed the whole changedHeader() function system of messages because all header changes now go through addHeader().
* Specific header implementations can be created for complex headers. When a message creates a header, it uses a
  HeaderFactory which can map specific headers to specific header classes. There is now a Link header and
  CacheControl header implementation.
* Removed from interface: Guzzle\Http\ClientInterface::setUriTemplate
* Removed from interface: Guzzle\Http\ClientInterface::setCurlMulti()
* Removed Guzzle\Http\Message\Request::receivedRequestHeader() and implemented this functionality in
  Guzzle\Http\Curl\RequestMediator
* Removed the optional $asString parameter from MessageInterface::getHeader(). Just cast the header to a string.
* Removed the optional $tryChunkedTransfer option from Guzzle\Http\Message\EntityEnclosingRequestInterface
* Removed the $asObjects argument from Guzzle\Http\Message\MessageInterface::getHeaders()
* Removed Guzzle\Parser\ParserRegister::get(). Use getParser()
* Removed Guzzle\Parser\ParserRegister::set(). Use registerParser().
* All response header helper functions return a string rather than mixing Header objects and strings inconsistently
* Removed cURL blacklist support. This is no longer necessary now that Expect, Accept, etc are managed by Guzzle
  directly via interfaces
* Removed the injecting of a request object onto a response object. The methods to get and set a request still exist
  but are a no-op until removed.
* Most classes that used to require a ``Guzzle\Service\Command\CommandInterface` typehint now request a
  `Guzzle\Service\Command\ArrayCommandInterface`.
* Added `Guzzle\Http\Message\RequestInterface::startResponse()` to the RequestInterface to handle injecting a response
  on a request while the request is still being transferred
* The ability to case-insensitively search for header values
* Guzzle\Http\Message\Header::hasExactHeader
* Guzzle\Http\Message\Header::raw. Use getAll()
* Deprecated cache control specific methods on Guzzle\Http\Message\AbstractMessage. Use the CacheControl header object
  instead.
* `Guzzle\Service\Command\CommandInterface` now extends from ToArrayInterface and ArrayAccess
* Added the ability to cast Model objects to a string to view debug information.

## 3.5.0 - 2013-05-13

* Bug: Fixed a regression so that request responses are parsed only once per oncomplete event rather than multiple times
* Bug: Better cleanup of one-time events accross the board (when an event is meant to fire once, it will now remove
  itself from the EventDispatcher)
* Bug: `Guzzle\Log\MessageFormatter` now properly writes "total_time" and "connect_time" values
* Bug: Cloning an EntityEnclosingRequest now clones the EntityBody too
* Bug: Fixed an undefined index error when parsing nested JSON responses with a sentAs parameter that reference a
  non-existent key
* Bug: All __call() method arguments are now required (helps with mocking frameworks)
* Deprecating Response::getRequest() and now using a shallow clone of a request object to remove a circular reference
  to help with refcount based garbage collection of resources created by sending a request
* Deprecating ZF1 cache and log adapters. These will be removed in the next major version.
* Deprecating `Response::getPreviousResponse()` (method signature still exists, but it'sdeprecated). Use the
  HistoryPlugin for a history.
* Added a `responseBody` alias for the `response_body` location
* Refactored internals to no longer rely on Response::getRequest()
* HistoryPlugin can now be cast to a string
* HistoryPlugin now logs transactions rather than requests and responses to more accurately keep track of the requests
  and responses that are sent over the wire
* Added `getEffectiveUrl()` and `getRedirectCount()` to Response objects

## 3.4.3 - 2013-04-30

* Bug fix: Fixing bug introduced in 3.4.2 where redirect responses are duplicated on the final redirected response
* Added a check to re-extract the temp cacert bundle from the phar before sending each request

## 3.4.2 - 2013-04-29

* Bug fix: Stream objects now work correctly with "a" and "a+" modes
* Bug fix: Removing `Transfer-Encoding: chunked` header when a Content-Length is present
* Bug fix: AsyncPlugin no longer forces HEAD requests
* Bug fix: DateTime timezones are now properly handled when using the service description schema formatter
* Bug fix: CachePlugin now properly handles stale-if-error directives when a request to the origin server fails
* Setting a response on a request will write to the custom request body from the response body if one is specified
* LogPlugin now writes to php://output when STDERR is undefined
* Added the ability to set multiple POST files for the same key in a single call
* application/x-www-form-urlencoded POSTs now use the utf-8 charset by default
* Added the ability to queue CurlExceptions to the MockPlugin
* Cleaned up how manual responses are queued on requests (removed "queued_response" and now using request.before_send)
* Configuration loading now allows remote files

## 3.4.1 - 2013-04-16

* Large refactoring to how CurlMulti handles work. There is now a proxy that sits in front of a pool of CurlMulti
  handles. This greatly simplifies the implementation, fixes a couple bugs, and provides a small performance boost.
* Exceptions are now properly grouped when sending requests in parallel
* Redirects are now properly aggregated when a multi transaction fails
* Redirects now set the response on the original object even in the event of a failure
* Bug fix: Model names are now properly set even when using $refs
* Added support for PHP 5.5's CurlFile to prevent warnings with the deprecated @ syntax
* Added support for oauth_callback in OAuth signatures
* Added support for oauth_verifier in OAuth signatures
* Added support to attempt to retrieve a command first literally, then ucfirst, the with inflection

## 3.4.0 - 2013-04-11

* Bug fix: URLs are now resolved correctly based on http://tools.ietf.org/html/rfc3986#section-5.2. #289
* Bug fix: Absolute URLs with a path in a service description will now properly override the base URL. #289
* Bug fix: Parsing a query string with a single PHP array value will now result in an array. #263
* Bug fix: Better normalization of the User-Agent header to prevent duplicate headers. #264.
* Bug fix: Added `number` type to service descriptions.
* Bug fix: empty parameters are removed from an OAuth signature
* Bug fix: Revalidating a cache entry prefers the Last-Modified over the Date header
* Bug fix: Fixed "array to string" error when validating a union of types in a service description
* Bug fix: Removed code that attempted to determine the size of a stream when data is written to the stream
* Bug fix: Not including an `oauth_token` if the value is null in the OauthPlugin.
* Bug fix: Now correctly aggregating successful requests and failed requests in CurlMulti when a redirect occurs.
* The new default CURLOPT_TIMEOUT setting has been increased to 150 seconds so that Guzzle works on poor connections.
* Added a feature to EntityEnclosingRequest::setBody() that will automatically set the Content-Type of the request if
  the Content-Type can be determined based on the entity body or the path of the request.
* Added the ability to overwrite configuration settings in a client when grabbing a throwaway client from a builder.
* Added support for a PSR-3 LogAdapter.
* Added a `command.after_prepare` event
* Added `oauth_callback` parameter to the OauthPlugin
* Added the ability to create a custom stream class when using a stream factory
* Added a CachingEntityBody decorator
* Added support for `additionalParameters` in service descriptions to define how custom parameters are serialized.
* The bundled SSL certificate is now provided in the phar file and extracted when running Guzzle from a phar.
* You can now send any EntityEnclosingRequest with POST fields or POST files and cURL will handle creating bodies
* POST requests using a custom entity body are now treated exactly like PUT requests but with a custom cURL method. This
  means that the redirect behavior of POST requests with custom bodies will not be the same as POST requests that use
  POST fields or files (the latter is only used when emulating a form POST in the browser).
* Lots of cleanup to CurlHandle::factory and RequestFactory::createRequest

## 3.3.1 - 2013-03-10

* Added the ability to create PHP streaming responses from HTTP requests
* Bug fix: Running any filters when parsing response headers with service descriptions
* Bug fix: OauthPlugin fixes to allow for multi-dimensional array signing, and sorting parameters before signing
* Bug fix: Removed the adding of default empty arrays and false Booleans to responses in order to be consistent across
  response location visitors.
* Bug fix: Removed the possibility of creating configuration files with circular dependencies
* RequestFactory::create() now uses the key of a POST file when setting the POST file name
* Added xmlAllowEmpty to serialize an XML body even if no XML specific parameters are set

## 3.3.0 - 2013-03-03

* A large number of performance optimizations have been made
* Bug fix: Added 'wb' as a valid write mode for streams
* Bug fix: `Guzzle\Http\Message\Response::json()` now allows scalar values to be returned
* Bug fix: Fixed bug in `Guzzle\Http\Message\Response` where wrapping quotes were stripped from `getEtag()`
* BC: Removed `Guzzle\Http\Utils` class
* BC: Setting a service description on a client will no longer modify the client's command factories.
* BC: Emitting IO events from a RequestMediator is now a parameter that must be set in a request's curl options using
  the 'emit_io' key. This was previously set under a request's parameters using 'curl.emit_io'
* BC: `Guzzle\Stream\Stream::getWrapper()` and `Guzzle\Stream\Stream::getSteamType()` are no longer converted to
  lowercase
* Operation parameter objects are now lazy loaded internally
* Added ErrorResponsePlugin that can throw errors for responses defined in service description operations' errorResponses
* Added support for instantiating responseType=class responseClass classes. Classes must implement
  `Guzzle\Service\Command\ResponseClassInterface`
* Added support for additionalProperties for top-level parameters in responseType=model responseClasses. These
  additional properties also support locations and can be used to parse JSON responses where the outermost part of the
  JSON is an array
* Added support for nested renaming of JSON models (rename sentAs to name)
* CachePlugin
    * Added support for stale-if-error so that the CachePlugin can now serve stale content from the cache on error
    * Debug headers can now added to cached response in the CachePlugin

## 3.2.0 - 2013-02-14

* CurlMulti is no longer reused globally. A new multi object is created per-client. This helps to isolate clients.
* URLs with no path no longer contain a "/" by default
* Guzzle\Http\QueryString does no longer manages the leading "?". This is now handled in Guzzle\Http\Url.
* BadResponseException no longer includes the full request and response message
* Adding setData() to Guzzle\Service\Description\ServiceDescriptionInterface
* Adding getResponseBody() to Guzzle\Http\Message\RequestInterface
* Various updates to classes to use ServiceDescriptionInterface type hints rather than ServiceDescription
* Header values can now be normalized into distinct values when multiple headers are combined with a comma separated list
* xmlEncoding can now be customized for the XML declaration of a XML service description operation
* Guzzle\Http\QueryString now uses Guzzle\Http\QueryAggregator\QueryAggregatorInterface objects to add custom value
  aggregation and no longer uses callbacks
* The URL encoding implementation of Guzzle\Http\QueryString can now be customized
* Bug fix: Filters were not always invoked for array service description parameters
* Bug fix: Redirects now use a target response body rather than a temporary response body
* Bug fix: The default exponential backoff BackoffPlugin was not giving when the request threshold was exceeded
* Bug fix: Guzzle now takes the first found value when grabbing Cache-Control directives

## 3.1.2 - 2013-01-27

* Refactored how operation responses are parsed. Visitors now include a before() method responsible for parsing the
  response body. For example, the XmlVisitor now parses the XML response into an array in the before() method.
* Fixed an issue where cURL would not automatically decompress responses when the Accept-Encoding header was sent
* CURLOPT_SSL_VERIFYHOST is never set to 1 because it is deprecated (see 5e0ff2ef20f839e19d1eeb298f90ba3598784444)
* Fixed a bug where redirect responses were not chained correctly using getPreviousResponse()
* Setting default headers on a client after setting the user-agent will not erase the user-agent setting

## 3.1.1 - 2013-01-20

* Adding wildcard support to Guzzle\Common\Collection::getPath()
* Adding alias support to ServiceBuilder configs
* Adding Guzzle\Service\Resource\CompositeResourceIteratorFactory and cleaning up factory interface

## 3.1.0 - 2013-01-12

* BC: CurlException now extends from RequestException rather than BadResponseException
* BC: Renamed Guzzle\Plugin\Cache\CanCacheStrategyInterface::canCache() to canCacheRequest() and added CanCacheResponse()
* Added getData to ServiceDescriptionInterface
* Added context array to RequestInterface::setState()
* Bug: Removing hard dependency on the BackoffPlugin from Guzzle\Http
* Bug: Adding required content-type when JSON request visitor adds JSON to a command
* Bug: Fixing the serialization of a service description with custom data
* Made it easier to deal with exceptions thrown when transferring commands or requests in parallel by providing
  an array of successful and failed responses
* Moved getPath from Guzzle\Service\Resource\Model to Guzzle\Common\Collection
* Added Guzzle\Http\IoEmittingEntityBody
* Moved command filtration from validators to location visitors
* Added `extends` attributes to service description parameters
* Added getModels to ServiceDescriptionInterface

## 3.0.7 - 2012-12-19

* Fixing phar detection when forcing a cacert to system if null or true
* Allowing filename to be passed to `Guzzle\Http\Message\Request::setResponseBody()`
* Cleaning up `Guzzle\Common\Collection::inject` method
* Adding a response_body location to service descriptions

## 3.0.6 - 2012-12-09

* CurlMulti performance improvements
* Adding setErrorResponses() to Operation
* composer.json tweaks

## 3.0.5 - 2012-11-18

* Bug: Fixing an infinite recursion bug caused from revalidating with the CachePlugin
* Bug: Response body can now be a string containing "0"
* Bug: Using Guzzle inside of a phar uses system by default but now allows for a custom cacert
* Bug: QueryString::fromString now properly parses query string parameters that contain equal signs
* Added support for XML attributes in service description responses
* DefaultRequestSerializer now supports array URI parameter values for URI template expansion
* Added better mimetype guessing to requests and post files

## 3.0.4 - 2012-11-11

* Bug: Fixed a bug when adding multiple cookies to a request to use the correct glue value
* Bug: Cookies can now be added that have a name, domain, or value set to "0"
* Bug: Using the system cacert bundle when using the Phar
* Added json and xml methods to Response to make it easier to parse JSON and XML response data into data structures
* Enhanced cookie jar de-duplication
* Added the ability to enable strict cookie jars that throw exceptions when invalid cookies are added
* Added setStream to StreamInterface to actually make it possible to implement custom rewind behavior for entity bodies
* Added the ability to create any sort of hash for a stream rather than just an MD5 hash

## 3.0.3 - 2012-11-04

* Implementing redirects in PHP rather than cURL
* Added PECL URI template extension and using as default parser if available
* Bug: Fixed Content-Length parsing of Response factory
* Adding rewind() method to entity bodies and streams. Allows for custom rewinding of non-repeatable streams.
* Adding ToArrayInterface throughout library
* Fixing OauthPlugin to create unique nonce values per request

## 3.0.2 - 2012-10-25

* Magic methods are enabled by default on clients
* Magic methods return the result of a command
* Service clients no longer require a base_url option in the factory
* Bug: Fixed an issue with URI templates where null template variables were being expanded

## 3.0.1 - 2012-10-22

* Models can now be used like regular collection objects by calling filter, map, etc
* Models no longer require a Parameter structure or initial data in the constructor
* Added a custom AppendIterator to get around a PHP bug with the `\AppendIterator`

## 3.0.0 - 2012-10-15

* Rewrote service description format to be based on Swagger
    * Now based on JSON schema
    * Added nested input structures and nested response models
    * Support for JSON and XML input and output models
    * Renamed `commands` to `operations`
    * Removed dot class notation
    * Removed custom types
* Broke the project into smaller top-level namespaces to be more component friendly
* Removed support for XML configs and descriptions. Use arrays or JSON files.
* Removed the Validation component and Inspector
* Moved all cookie code to Guzzle\Plugin\Cookie
* Magic methods on a Guzzle\Service\Client now return the command un-executed.
* Calling getResult() or getResponse() on a command will lazily execute the command if needed.
* Now shipping with cURL's CA certs and using it by default
* Added previousResponse() method to response objects
* No longer sending Accept and Accept-Encoding headers on every request
* Only sending an Expect header by default when a payload is greater than 1MB
* Added/moved client options:
    * curl.blacklist to curl.option.blacklist
    * Added ssl.certificate_authority
* Added a Guzzle\Iterator component
* Moved plugins from Guzzle\Http\Plugin to Guzzle\Plugin
* Added a more robust backoff retry strategy (replaced the ExponentialBackoffPlugin)
* Added a more robust caching plugin
* Added setBody to response objects
* Updating LogPlugin to use a more flexible MessageFormatter
* Added a completely revamped build process
* Cleaning up Collection class and removing default values from the get method
* Fixed ZF2 cache adapters

## 2.8.8 - 2012-10-15

* Bug: Fixed a cookie issue that caused dot prefixed domains to not match where popular browsers did

## 2.8.7 - 2012-09-30

* Bug: Fixed config file aliases for JSON includes
* Bug: Fixed cookie bug on a request object by using CookieParser to parse cookies on requests
* Bug: Removing the path to a file when sending a Content-Disposition header on a POST upload
* Bug: Hardening request and response parsing to account for missing parts
* Bug: Fixed PEAR packaging
* Bug: Fixed Request::getInfo
* Bug: Fixed cases where CURLM_CALL_MULTI_PERFORM return codes were causing curl transactions to fail
* Adding the ability for the namespace Iterator factory to look in multiple directories
* Added more getters/setters/removers from service descriptions
* Added the ability to remove POST fields from OAuth signatures
* OAuth plugin now supports 2-legged OAuth

## 2.8.6 - 2012-09-05

* Added the ability to modify and build service descriptions
* Added the use of visitors to apply parameters to locations in service descriptions using the dynamic command
* Added a `json` parameter location
* Now allowing dot notation for classes in the CacheAdapterFactory
* Using the union of two arrays rather than an array_merge when extending service builder services and service params
* Ensuring that a service is a string before doing strpos() checks on it when substituting services for references
  in service builder config files.
* Services defined in two different config files that include one another will by default replace the previously
  defined service, but you can now create services that extend themselves and merge their settings over the previous
* The JsonLoader now supports aliasing filenames with different filenames. This allows you to alias something like
  '_default' with a default JSON configuration file.

## 2.8.5 - 2012-08-29

* Bug: Suppressed empty arrays from URI templates
* Bug: Added the missing $options argument from ServiceDescription::factory to enable caching
* Added support for HTTP responses that do not contain a reason phrase in the start-line
* AbstractCommand commands are now invokable
* Added a way to get the data used when signing an Oauth request before a request is sent

## 2.8.4 - 2012-08-15

* Bug: Custom delay time calculations are no longer ignored in the ExponentialBackoffPlugin
* Added the ability to transfer entity bodies as a string rather than streamed. This gets around curl error 65. Set `body_as_string` in a request's curl options to enable.
* Added a StreamInterface, EntityBodyInterface, and added ftell() to Guzzle\Common\Stream
* Added an AbstractEntityBodyDecorator and a ReadLimitEntityBody decorator to transfer only a subset of a decorated stream
* Stream and EntityBody objects will now return the file position to the previous position after a read required operation (e.g. getContentMd5())
* Added additional response status codes
* Removed SSL information from the default User-Agent header
* DELETE requests can now send an entity body
* Added an EventDispatcher to the ExponentialBackoffPlugin and added an ExponentialBackoffLogger to log backoff retries
* Added the ability of the MockPlugin to consume mocked request bodies
* LogPlugin now exposes request and response objects in the extras array

## 2.8.3 - 2012-07-30

* Bug: Fixed a case where empty POST requests were sent as GET requests
* Bug: Fixed a bug in ExponentialBackoffPlugin that caused fatal errors when retrying an EntityEnclosingRequest that does not have a body
* Bug: Setting the response body of a request to null after completing a request, not when setting the state of a request to new
* Added multiple inheritance to service description commands
* Added an ApiCommandInterface and added ``getParamNames()`` and ``hasParam()``
* Removed the default 2mb size cutoff from the Md5ValidatorPlugin so that it now defaults to validating everything
* Changed CurlMulti::perform to pass a smaller timeout to CurlMulti::executeHandles

## 2.8.2 - 2012-07-24

* Bug: Query string values set to 0 are no longer dropped from the query string
* Bug: A Collection object is no longer created each time a call is made to ``Guzzle\Service\Command\AbstractCommand::getRequestHeaders()``
* Bug: ``+`` is now treated as an encoded space when parsing query strings
* QueryString and Collection performance improvements
* Allowing dot notation for class paths in filters attribute of a service descriptions

## 2.8.1 - 2012-07-16

* Loosening Event Dispatcher dependency
* POST redirects can now be customized using CURLOPT_POSTREDIR

## 2.8.0 - 2012-07-15

* BC: Guzzle\Http\Query
    * Query strings with empty variables will always show an equal sign unless the variable is set to QueryString::BLANK (e.g. ?acl= vs ?acl)
    * Changed isEncodingValues() and isEncodingFields() to isUrlEncoding()
    * Changed setEncodeValues(bool) and setEncodeFields(bool) to useUrlEncoding(bool)
    * Changed the aggregation functions of QueryString to be static methods
    * Can now use fromString() with querystrings that have a leading ?
* cURL configuration values can be specified in service descriptions using ``curl.`` prefixed parameters
* Content-Length is set to 0 before emitting the request.before_send event when sending an empty request body
* Cookies are no longer URL decoded by default
* Bug: URI template variables set to null are no longer expanded

## 2.7.2 - 2012-07-02

* BC: Moving things to get ready for subtree splits. Moving Inflection into Common. Moving Guzzle\Http\Parser to Guzzle\Parser.
* BC: Removing Guzzle\Common\Batch\Batch::count() and replacing it with isEmpty()
* CachePlugin now allows for a custom request parameter function to check if a request can be cached
* Bug fix: CachePlugin now only caches GET and HEAD requests by default
* Bug fix: Using header glue when transferring headers over the wire
* Allowing deeply nested arrays for composite variables in URI templates
* Batch divisors can now return iterators or arrays

## 2.7.1 - 2012-06-26

* Minor patch to update version number in UA string
* Updating build process

## 2.7.0 - 2012-06-25

* BC: Inflection classes moved to Guzzle\Inflection. No longer static methods. Can now inject custom inflectors into classes.
* BC: Removed magic setX methods from commands
* BC: Magic methods mapped to service description commands are now inflected in the command factory rather than the client __call() method
* Verbose cURL options are no longer enabled by default. Set curl.debug to true on a client to enable.
* Bug: Now allowing colons in a response start-line (e.g. HTTP/1.1 503 Service Unavailable: Back-end server is at capacity)
* Guzzle\Service\Resource\ResourceIteratorApplyBatched now internally uses the Guzzle\Common\Batch namespace
* Added Guzzle\Service\Plugin namespace and a PluginCollectionPlugin
* Added the ability to set POST fields and files in a service description
* Guzzle\Http\EntityBody::factory() now accepts objects with a __toString() method
* Adding a command.before_prepare event to clients
* Added BatchClosureTransfer and BatchClosureDivisor
* BatchTransferException now includes references to the batch divisor and transfer strategies
* Fixed some tests so that they pass more reliably
* Added Guzzle\Common\Log\ArrayLogAdapter

## 2.6.6 - 2012-06-10

* BC: Removing Guzzle\Http\Plugin\BatchQueuePlugin
* BC: Removing Guzzle\Service\Command\CommandSet
* Adding generic batching system (replaces the batch queue plugin and command set)
* Updating ZF cache and log adapters and now using ZF's composer repository
* Bug: Setting the name of each ApiParam when creating through an ApiCommand
* Adding result_type, result_doc, deprecated, and doc_url to service descriptions
* Bug: Changed the default cookie header casing back to 'Cookie'

## 2.6.5 - 2012-06-03

* BC: Renaming Guzzle\Http\Message\RequestInterface::getResourceUri() to getResource()
* BC: Removing unused AUTH_BASIC and AUTH_DIGEST constants from
* BC: Guzzle\Http\Cookie is now used to manage Set-Cookie data, not Cookie data
* BC: Renaming methods in the CookieJarInterface
* Moving almost all cookie logic out of the CookiePlugin and into the Cookie or CookieJar implementations
* Making the default glue for HTTP headers ';' instead of ','
* Adding a removeValue to Guzzle\Http\Message\Header
* Adding getCookies() to request interface.
* Making it easier to add event subscribers to HasDispatcherInterface classes. Can now directly call addSubscriber()

## 2.6.4 - 2012-05-30

* BC: Cleaning up how POST files are stored in EntityEnclosingRequest objects. Adding PostFile class.
* BC: Moving ApiCommand specific functionality from the Inspector and on to the ApiCommand
* Bug: Fixing magic method command calls on clients
* Bug: Email constraint only validates strings
* Bug: Aggregate POST fields when POST files are present in curl handle
* Bug: Fixing default User-Agent header
* Bug: Only appending or prepending parameters in commands if they are specified
* Bug: Not requiring response reason phrases or status codes to match a predefined list of codes
* Allowing the use of dot notation for class namespaces when using instance_of constraint
* Added any_match validation constraint
* Added an AsyncPlugin
* Passing request object to the calculateWait method of the ExponentialBackoffPlugin
* Allowing the result of a command object to be changed
* Parsing location and type sub values when instantiating a service description rather than over and over at runtime

## 2.6.3 - 2012-05-23

* [BC] Guzzle\Common\FromConfigInterface no longer requires any config options.
* [BC] Refactoring how POST files are stored on an EntityEnclosingRequest. They are now separate from POST fields.
* You can now use an array of data when creating PUT request bodies in the request factory.
* Removing the requirement that HTTPS requests needed a Cache-Control: public directive to be cacheable.
* [Http] Adding support for Content-Type in multipart POST uploads per upload
* [Http] Added support for uploading multiple files using the same name (foo[0], foo[1])
* Adding more POST data operations for easier manipulation of POST data.
* You can now set empty POST fields.
* The body of a request is only shown on EntityEnclosingRequest objects that do not use POST files.
* Split the Guzzle\Service\Inspector::validateConfig method into two methods. One to initialize when a command is created, and one to validate.
* CS updates

## 2.6.2 - 2012-05-19

* [Http] Better handling of nested scope requests in CurlMulti.  Requests are now always prepares in the send() method rather than the addRequest() method.

## 2.6.1 - 2012-05-19

* [BC] Removing 'path' support in service descriptions.  Use 'uri'.
* [BC] Guzzle\Service\Inspector::parseDocBlock is now protected. Adding getApiParamsForClass() with cache.
* [BC] Removing Guzzle\Common\NullObject.  Use https://github.com/mtdowling/NullObject if you need it.
* [BC] Removing Guzzle\Common\XmlElement.
* All commands, both dynamic and concrete, have ApiCommand objects.
* Adding a fix for CurlMulti so that if all of the connections encounter some sort of curl error, then the loop exits.
* Adding checks to EntityEnclosingRequest so that empty POST files and fields are ignored.
* Making the method signature of Guzzle\Service\Builder\ServiceBuilder::factory more flexible.

## 2.6.0 - 2012-05-15

* [BC] Moving Guzzle\Service\Builder to Guzzle\Service\Builder\ServiceBuilder
* [BC] Executing a Command returns the result of the command rather than the command
* [BC] Moving all HTTP parsing logic to Guzzle\Http\Parsers. Allows for faster C implementations if needed.
* [BC] Changing the Guzzle\Http\Message\Response::setProtocol() method to accept a protocol and version in separate args.
* [BC] Moving ResourceIterator* to Guzzle\Service\Resource
* [BC] Completely refactored ResourceIterators to iterate over a cloned command object
* [BC] Moved Guzzle\Http\UriTemplate to Guzzle\Http\Parser\UriTemplate\UriTemplate
* [BC] Guzzle\Guzzle is now deprecated
* Moving Guzzle\Common\Guzzle::inject to Guzzle\Common\Collection::inject
* Adding Guzzle\Version class to give version information about Guzzle
* Adding Guzzle\Http\Utils class to provide getDefaultUserAgent() and getHttpDate()
* Adding Guzzle\Curl\CurlVersion to manage caching curl_version() data
* ServiceDescription and ServiceBuilder are now cacheable using similar configs
* Changing the format of XML and JSON service builder configs.  Backwards compatible.
* Cleaned up Cookie parsing
* Trimming the default Guzzle User-Agent header
* Adding a setOnComplete() method to Commands that is called when a command completes
* Keeping track of requests that were mocked in the MockPlugin
* Fixed a caching bug in the CacheAdapterFactory
* Inspector objects can be injected into a Command object
* Refactoring a lot of code and tests to be case insensitive when dealing with headers
* Adding Guzzle\Http\Message\HeaderComparison for easy comparison of HTTP headers using a DSL
* Adding the ability to set global option overrides to service builder configs
* Adding the ability to include other service builder config files from within XML and JSON files
* Moving the parseQuery method out of Url and on to QueryString::fromString() as a static factory method.

## 2.5.0 - 2012-05-08

* Major performance improvements
* [BC] Simplifying Guzzle\Common\Collection.  Please check to see if you are using features that are now deprecated.
* [BC] Using a custom validation system that allows a flyweight implementation for much faster validation. No longer using Symfony2 Validation component.
* [BC] No longer supporting "{{ }}" for injecting into command or UriTemplates.  Use "{}"
* Added the ability to passed parameters to all requests created by a client
* Added callback functionality to the ExponentialBackoffPlugin
* Using microtime in ExponentialBackoffPlugin to allow more granular backoff strategies.
* Rewinding request stream bodies when retrying requests
* Exception is thrown when JSON response body cannot be decoded
* Added configurable magic method calls to clients and commands.  This is off by default.
* Fixed a defect that added a hash to every parsed URL part
* Fixed duplicate none generation for OauthPlugin.
* Emitting an event each time a client is generated by a ServiceBuilder
* Using an ApiParams object instead of a Collection for parameters of an ApiCommand
* cache.* request parameters should be renamed to params.cache.*
* Added the ability to set arbitrary curl options on requests (disable_wire, progress, etc). See CurlHandle.
* Added the ability to disable type validation of service descriptions
* ServiceDescriptions and ServiceBuilders are now Serializable
