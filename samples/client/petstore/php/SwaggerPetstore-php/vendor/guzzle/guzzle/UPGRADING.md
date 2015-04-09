Guzzle Upgrade Guide
====================

3.6 to 3.7
----------

### Deprecations

- You can now enable E_USER_DEPRECATED warnings to see if you are using any deprecated methods.:

```php
\Guzzle\Common\Version::$emitWarnings = true;
```

The following APIs and options have been marked as deprecated:

- Marked `Guzzle\Http\Message\Request::isResponseBodyRepeatable()` as deprecated. Use `$request->getResponseBody()->isRepeatable()` instead.
- Marked `Guzzle\Http\Message\Request::canCache()` as deprecated. Use `Guzzle\Plugin\Cache\DefaultCanCacheStrategy->canCacheRequest()` instead.
- Marked `Guzzle\Http\Message\Request::canCache()` as deprecated. Use `Guzzle\Plugin\Cache\DefaultCanCacheStrategy->canCacheRequest()` instead.
- Marked `Guzzle\Http\Message\Request::setIsRedirect()` as deprecated. Use the HistoryPlugin instead.
- Marked `Guzzle\Http\Message\Request::isRedirect()` as deprecated. Use the HistoryPlugin instead.
- Marked `Guzzle\Cache\CacheAdapterFactory::factory()` as deprecated
- Marked `Guzzle\Service\Client::enableMagicMethods()` as deprecated. Magic methods can no longer be disabled on a Guzzle\Service\Client.
- Marked `Guzzle\Parser\Url\UrlParser` as deprecated. Just use PHP's `parse_url()` and percent encode your UTF-8.
- Marked `Guzzle\Common\Collection::inject()` as deprecated.
- Marked `Guzzle\Plugin\CurlAuth\CurlAuthPlugin` as deprecated. Use
  `$client->getConfig()->setPath('request.options/auth', array('user', 'pass', 'Basic|Digest|NTLM|Any'));` or
  `$client->setDefaultOption('auth', array('user', 'pass', 'Basic|Digest|NTLM|Any'));`

3.7 introduces `request.options` as a parameter for a client configuration and as an optional argument to all creational
request methods. When paired with a client's configuration settings, these options allow you to specify default settings
for various aspects of a request. Because these options make other previous configuration options redundant, several
configuration options and methods of a client and AbstractCommand have been deprecated.

- Marked `Guzzle\Service\Client::getDefaultHeaders()` as deprecated. Use `$client->getDefaultOption('headers')`.
- Marked `Guzzle\Service\Client::setDefaultHeaders()` as deprecated. Use `$client->setDefaultOption('headers/{header_name}', 'value')`.
- Marked 'request.params' for `Guzzle\Http\Client` as deprecated. Use `$client->setDefaultOption('params/{param_name}', 'value')`
- Marked 'command.headers', 'command.response_body' and 'command.on_complete' as deprecated for AbstractCommand. These will work through Guzzle 4.0

        $command = $client->getCommand('foo', array(
            'command.headers' => array('Test' => '123'),
            'command.response_body' => '/path/to/file'
        ));

        // Should be changed to:

        $command = $client->getCommand('foo', array(
            'command.request_options' => array(
                'headers' => array('Test' => '123'),
                'save_as' => '/path/to/file'
            )
        ));

### Interface changes

Additions and changes (you will need to update any implementations or subclasses you may have created):

- Added an `$options` argument to the end of the following methods of `Guzzle\Http\ClientInterface`:
  createRequest, head, delete, put, patch, post, options, prepareRequest
- Added an `$options` argument to the end of `Guzzle\Http\Message\Request\RequestFactoryInterface::createRequest()`
- Added an `applyOptions()` method to `Guzzle\Http\Message\Request\RequestFactoryInterface`
- Changed `Guzzle\Http\ClientInterface::get($uri = null, $headers = null, $body = null)` to
  `Guzzle\Http\ClientInterface::get($uri = null, $headers = null, $options = array())`. You can still pass in a
  resource, string, or EntityBody into the $options parameter to specify the download location of the response.
- Changed `Guzzle\Common\Collection::__construct($data)` to no longer accepts a null value for `$data` but a
  default `array()`
- Added `Guzzle\Stream\StreamInterface::isRepeatable`
- Made `Guzzle\Http\Client::expandTemplate` and `getUriTemplate` protected methods.

The following methods were removed from interfaces. All of these methods are still available in the concrete classes
that implement them, but you should update your code to use alternative methods:

- Removed `Guzzle\Http\ClientInterface::setDefaultHeaders(). Use
  `$client->getConfig()->setPath('request.options/headers/{header_name}', 'value')`. or
  `$client->getConfig()->setPath('request.options/headers', array('header_name' => 'value'))` or
  `$client->setDefaultOption('headers/{header_name}', 'value')`. or
  `$client->setDefaultOption('headers', array('header_name' => 'value'))`.
- Removed `Guzzle\Http\ClientInterface::getDefaultHeaders(). Use `$client->getConfig()->getPath('request.options/headers')`.
- Removed `Guzzle\Http\ClientInterface::expandTemplate()`. This is an implementation detail.
- Removed `Guzzle\Http\ClientInterface::setRequestFactory()`. This is an implementation detail.
- Removed `Guzzle\Http\ClientInterface::getCurlMulti()`. This is a very specific implementation detail.
- Removed `Guzzle\Http\Message\RequestInterface::canCache`. Use the CachePlugin.
- Removed `Guzzle\Http\Message\RequestInterface::setIsRedirect`. Use the HistoryPlugin.
- Removed `Guzzle\Http\Message\RequestInterface::isRedirect`. Use the HistoryPlugin.

### Cache plugin breaking changes

- CacheKeyProviderInterface and DefaultCacheKeyProvider are no longer used. All of this logic is handled in a
  CacheStorageInterface. These two objects and interface will be removed in a future version.
- Always setting X-cache headers on cached responses
- Default cache TTLs are now handled by the CacheStorageInterface of a CachePlugin
- `CacheStorageInterface::cache($key, Response $response, $ttl = null)` has changed to `cache(RequestInterface
  $request, Response $response);`
- `CacheStorageInterface::fetch($key)` has changed to `fetch(RequestInterface $request);`
- `CacheStorageInterface::delete($key)` has changed to `delete(RequestInterface $request);`
- Added `CacheStorageInterface::purge($url)`
- `DefaultRevalidation::__construct(CacheKeyProviderInterface $cacheKey, CacheStorageInterface $cache, CachePlugin
  $plugin)` has changed to `DefaultRevalidation::__construct(CacheStorageInterface $cache,
  CanCacheStrategyInterface $canCache = null)`
- Added `RevalidationInterface::shouldRevalidate(RequestInterface $request, Response $response)`

3.5 to 3.6
----------

* Mixed casing of headers are now forced to be a single consistent casing across all values for that header.
* Messages internally use a HeaderCollection object to delegate handling case-insensitive header resolution
* Removed the whole changedHeader() function system of messages because all header changes now go through addHeader().
  For example, setHeader() first removes the header using unset on a HeaderCollection and then calls addHeader().
  Keeping the Host header and URL host in sync is now handled by overriding the addHeader method in Request.
* Specific header implementations can be created for complex headers. When a message creates a header, it uses a
  HeaderFactory which can map specific headers to specific header classes. There is now a Link header and
  CacheControl header implementation.
* Moved getLinks() from Response to just be used on a Link header object.

If you previously relied on Guzzle\Http\Message\Header::raw(), then you will need to update your code to use the
HeaderInterface (e.g. toArray(), getAll(), etc).

### Interface changes

* Removed from interface: Guzzle\Http\ClientInterface::setUriTemplate
* Removed from interface: Guzzle\Http\ClientInterface::setCurlMulti()
* Removed Guzzle\Http\Message\Request::receivedRequestHeader() and implemented this functionality in
  Guzzle\Http\Curl\RequestMediator
* Removed the optional $asString parameter from MessageInterface::getHeader(). Just cast the header to a string.
* Removed the optional $tryChunkedTransfer option from Guzzle\Http\Message\EntityEnclosingRequestInterface
* Removed the $asObjects argument from Guzzle\Http\Message\MessageInterface::getHeaders()

### Removed deprecated functions

* Removed Guzzle\Parser\ParserRegister::get(). Use getParser()
* Removed Guzzle\Parser\ParserRegister::set(). Use registerParser().

### Deprecations

* The ability to case-insensitively search for header values
* Guzzle\Http\Message\Header::hasExactHeader
* Guzzle\Http\Message\Header::raw. Use getAll()
* Deprecated cache control specific methods on Guzzle\Http\Message\AbstractMessage. Use the CacheControl header object
  instead.

### Other changes

* All response header helper functions return a string rather than mixing Header objects and strings inconsistently
* Removed cURL blacklist support. This is no longer necessary now that Expect, Accept, etc are managed by Guzzle
  directly via interfaces
* Removed the injecting of a request object onto a response object. The methods to get and set a request still exist
  but are a no-op until removed.
* Most classes that used to require a ``Guzzle\Service\Command\CommandInterface` typehint now request a
  `Guzzle\Service\Command\ArrayCommandInterface`.
* Added `Guzzle\Http\Message\RequestInterface::startResponse()` to the RequestInterface to handle injecting a response
  on a request while the request is still being transferred
* `Guzzle\Service\Command\CommandInterface` now extends from ToArrayInterface and ArrayAccess

3.3 to 3.4
----------

Base URLs of a client now follow the rules of http://tools.ietf.org/html/rfc3986#section-5.2.2 when merging URLs.

3.2 to 3.3
----------

### Response::getEtag() quote stripping removed

`Guzzle\Http\Message\Response::getEtag()` no longer strips quotes around the ETag response header

### Removed `Guzzle\Http\Utils`

The `Guzzle\Http\Utils` class was removed. This class was only used for testing.

### Stream wrapper and type

`Guzzle\Stream\Stream::getWrapper()` and `Guzzle\Stream\Stream::getSteamType()` are no longer converted to lowercase.

### curl.emit_io became emit_io

Emitting IO events from a RequestMediator is now a parameter that must be set in a request's curl options using the
'emit_io' key. This was previously set under a request's parameters using 'curl.emit_io'

3.1 to 3.2
----------

### CurlMulti is no longer reused globally

Before 3.2, the same CurlMulti object was reused globally for each client. This can cause issue where plugins added
to a single client can pollute requests dispatched from other clients.

If you still wish to reuse the same CurlMulti object with each client, then you can add a listener to the
ServiceBuilder's `service_builder.create_client` event to inject a custom CurlMulti object into each client as it is
created.

```php
$multi = new Guzzle\Http\Curl\CurlMulti();
$builder = Guzzle\Service\Builder\ServiceBuilder::factory('/path/to/config.json');
$builder->addListener('service_builder.create_client', function ($event) use ($multi) {
    $event['client']->setCurlMulti($multi);
}
});
```

### No default path

URLs no longer have a default path value of '/' if no path was specified.

Before:

```php
$request = $client->get('http://www.foo.com');
echo $request->getUrl();
// >> http://www.foo.com/
```

After:

```php
$request = $client->get('http://www.foo.com');
echo $request->getUrl();
// >> http://www.foo.com
```

### Less verbose BadResponseException

The exception message for `Guzzle\Http\Exception\BadResponseException` no longer contains the full HTTP request and
response information. You can, however, get access to the request and response object by calling `getRequest()` or
`getResponse()` on the exception object.

### Query parameter aggregation

Multi-valued query parameters are no longer aggregated using a callback function. `Guzzle\Http\Query` now has a
setAggregator() method that accepts a `Guzzle\Http\QueryAggregator\QueryAggregatorInterface` object. This object is
responsible for handling the aggregation of multi-valued query string variables into a flattened hash.

2.8 to 3.x
----------

### Guzzle\Service\Inspector

Change `\Guzzle\Service\Inspector::fromConfig` to `\Guzzle\Common\Collection::fromConfig`

**Before**

```php
use Guzzle\Service\Inspector;

class YourClient extends \Guzzle\Service\Client
{
    public static function factory($config = array())
    {
        $default = array();
        $required = array('base_url', 'username', 'api_key');
        $config = Inspector::fromConfig($config, $default, $required);

        $client = new self(
            $config->get('base_url'),
            $config->get('username'),
            $config->get('api_key')
        );
        $client->setConfig($config);

        $client->setDescription(ServiceDescription::factory(__DIR__ . DIRECTORY_SEPARATOR . 'client.json'));

        return $client;
    }
```

**After**

```php
use Guzzle\Common\Collection;

class YourClient extends \Guzzle\Service\Client
{
    public static function factory($config = array())
    {
        $default = array();
        $required = array('base_url', 'username', 'api_key');
        $config = Collection::fromConfig($config, $default, $required);

        $client = new self(
            $config->get('base_url'),
            $config->get('username'),
            $config->get('api_key')
        );
        $client->setConfig($config);

        $client->setDescription(ServiceDescription::factory(__DIR__ . DIRECTORY_SEPARATOR . 'client.json'));

        return $client;
    }
```

### Convert XML Service Descriptions to JSON

**Before**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<client>
    <commands>
        <!-- Groups -->
        <command name="list_groups" method="GET" uri="groups.json">
            <doc>Get a list of groups</doc>
        </command>
        <command name="search_groups" method="GET" uri='search.json?query="{{query}} type:group"'>
            <doc>Uses a search query to get a list of groups</doc>
            <param name="query" type="string" required="true" />
        </command>
        <command name="create_group" method="POST" uri="groups.json">
            <doc>Create a group</doc>
            <param name="data" type="array" location="body" filters="json_encode" doc="Group JSON"/>
            <param name="Content-Type" location="header" static="application/json"/>
        </command>
        <command name="delete_group" method="DELETE" uri="groups/{{id}}.json">
            <doc>Delete a group by ID</doc>
            <param name="id" type="integer" required="true"/>
        </command>
        <command name="get_group" method="GET" uri="groups/{{id}}.json">
            <param name="id" type="integer" required="true"/>
        </command>
        <command name="update_group" method="PUT" uri="groups/{{id}}.json">
            <doc>Update a group</doc>
            <param name="id" type="integer" required="true"/>
            <param name="data" type="array" location="body" filters="json_encode" doc="Group JSON"/>
            <param name="Content-Type" location="header" static="application/json"/>
        </command>
    </commands>
</client>
```

**After**

```json
{
    "name":       "Zendesk REST API v2",
    "apiVersion": "2012-12-31",
    "description":"Provides access to Zendesk views, groups, tickets, ticket fields, and users",
    "operations": {
        "list_groups":  {
            "httpMethod":"GET",
            "uri":       "groups.json",
            "summary":   "Get a list of groups"
        },
        "search_groups":{
            "httpMethod":"GET",
            "uri":       "search.json?query=\"{query} type:group\"",
            "summary":   "Uses a search query to get a list of groups",
            "parameters":{
                "query":{
                    "location":   "uri",
                    "description":"Zendesk Search Query",
                    "type":       "string",
                    "required":   true
                }
            }
        },
        "create_group": {
            "httpMethod":"POST",
            "uri":       "groups.json",
            "summary":   "Create a group",
            "parameters":{
                "data":        {
                    "type":       "array",
                    "location":   "body",
                    "description":"Group JSON",
                    "filters":    "json_encode",
                    "required":   true
                },
                "Content-Type":{
                    "type":    "string",
                    "location":"header",
                    "static":  "application/json"
                }
            }
        },
        "delete_group": {
            "httpMethod":"DELETE",
            "uri":       "groups/{id}.json",
            "summary":   "Delete a group",
            "parameters":{
                "id":{
                    "location":   "uri",
                    "description":"Group to delete by ID",
                    "type":       "integer",
                    "required":   true
                }
            }
        },
        "get_group":    {
            "httpMethod":"GET",
            "uri":       "groups/{id}.json",
            "summary":   "Get a ticket",
            "parameters":{
                "id":{
                    "location":   "uri",
                    "description":"Group to get by ID",
                    "type":       "integer",
                    "required":   true
                }
            }
        },
        "update_group": {
            "httpMethod":"PUT",
            "uri":       "groups/{id}.json",
            "summary":   "Update a group",
            "parameters":{
                "id":          {
                    "location":   "uri",
                    "description":"Group to update by ID",
                    "type":       "integer",
                    "required":   true
                },
                "data":        {
                    "type":       "array",
                    "location":   "body",
                    "description":"Group JSON",
                    "filters":    "json_encode",
                    "required":   true
                },
                "Content-Type":{
                    "type":    "string",
                    "location":"header",
                    "static":  "application/json"
                }
            }
        }
}
```

### Guzzle\Service\Description\ServiceDescription

Commands are now called Operations

**Before**

```php
use Guzzle\Service\Description\ServiceDescription;

$sd = new ServiceDescription();
$sd->getCommands();     // @returns ApiCommandInterface[]
$sd->hasCommand($name);
$sd->getCommand($name); // @returns ApiCommandInterface|null
$sd->addCommand($command); // @param ApiCommandInterface $command
```

**After**

```php
use Guzzle\Service\Description\ServiceDescription;

$sd = new ServiceDescription();
$sd->getOperations();           // @returns OperationInterface[]
$sd->hasOperation($name);
$sd->getOperation($name);       // @returns OperationInterface|null
$sd->addOperation($operation);  // @param OperationInterface $operation
```

### Guzzle\Common\Inflection\Inflector

Namespace is now `Guzzle\Inflection\Inflector`

### Guzzle\Http\Plugin

Namespace is now `Guzzle\Plugin`. Many other changes occur within this namespace and are detailed in their own sections below.

### Guzzle\Http\Plugin\LogPlugin and Guzzle\Common\Log

Now `Guzzle\Plugin\Log\LogPlugin` and `Guzzle\Log` respectively.

**Before**

```php
use Guzzle\Common\Log\ClosureLogAdapter;
use Guzzle\Http\Plugin\LogPlugin;

/** @var \Guzzle\Http\Client */
$client;

// $verbosity is an integer indicating desired message verbosity level
$client->addSubscriber(new LogPlugin(new ClosureLogAdapter(function($m) { echo $m; }, $verbosity = LogPlugin::LOG_VERBOSE);
```

**After**

```php
use Guzzle\Log\ClosureLogAdapter;
use Guzzle\Log\MessageFormatter;
use Guzzle\Plugin\Log\LogPlugin;

/** @var \Guzzle\Http\Client */
$client;

// $format is a string indicating desired message format -- @see MessageFormatter
$client->addSubscriber(new LogPlugin(new ClosureLogAdapter(function($m) { echo $m; }, $format = MessageFormatter::DEBUG_FORMAT);
```

### Guzzle\Http\Plugin\CurlAuthPlugin

Now `Guzzle\Plugin\CurlAuth\CurlAuthPlugin`.

### Guzzle\Http\Plugin\ExponentialBackoffPlugin

Now `Guzzle\Plugin\Backoff\BackoffPlugin`, and other changes.

**Before**

```php
use Guzzle\Http\Plugin\ExponentialBackoffPlugin;

$backoffPlugin = new ExponentialBackoffPlugin($maxRetries, array_merge(
        ExponentialBackoffPlugin::getDefaultFailureCodes(), array(429)
    ));

$client->addSubscriber($backoffPlugin);
```

**After**

```php
use Guzzle\Plugin\Backoff\BackoffPlugin;
use Guzzle\Plugin\Backoff\HttpBackoffStrategy;

// Use convenient factory method instead -- see implementation for ideas of what
// you can do with chaining backoff strategies
$backoffPlugin = BackoffPlugin::getExponentialBackoff($maxRetries, array_merge(
        HttpBackoffStrategy::getDefaultFailureCodes(), array(429)
    ));
$client->addSubscriber($backoffPlugin);
```

### Known Issues

#### [BUG] Accept-Encoding header behavior changed unintentionally.

(See #217) (Fixed in 09daeb8c666fb44499a0646d655a8ae36456575e)

In version 2.8 setting the `Accept-Encoding` header would set the CURLOPT_ENCODING option, which permitted cURL to
properly handle gzip/deflate compressed responses from the server. In versions affected by this bug this does not happen.
See issue #217 for a workaround, or use a version containing the fix.
