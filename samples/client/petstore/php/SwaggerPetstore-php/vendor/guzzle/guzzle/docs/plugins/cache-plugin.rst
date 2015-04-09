=================
HTTP Cache plugin
=================

Guzzle can leverage HTTP's caching specifications using the ``Guzzle\Plugin\Cache\CachePlugin``. The CachePlugin
provides a private transparent proxy cache that caches HTTP responses. The caching logic, based on
`RFC 2616 <http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html>`_, uses HTTP headers to control caching behavior,
cache lifetime, and supports Vary, ETag, and Last-Modified based revalidation:

.. code-block:: php

    use Guzzle\Http\Client;
    use Doctrine\Common\Cache\FilesystemCache;
    use Guzzle\Cache\DoctrineCacheAdapter;
    use Guzzle\Plugin\Cache\CachePlugin;
    use Guzzle\Plugin\Cache\DefaultCacheStorage;

    $client = new Client('http://www.test.com/');

    $cachePlugin = new CachePlugin(array(
        'storage' => new DefaultCacheStorage(
            new DoctrineCacheAdapter(
                new FilesystemCache('/path/to/cache/files')
            )
        )
    ));

    // Add the cache plugin to the client object
    $client->addSubscriber($cachePlugin);
    $client->get('http://www.wikipedia.org/')->send();

    // The next request will revalidate against the origin server to see if it
    // has been modified. If a 304 response is received the response will be
    // served from cache
    $client->get('http://www.wikipedia.org/')->send();

The cache plugin intercepts GET and HEAD requests before they are actually transferred to the origin server. The cache
plugin then generates a hash key based on the request method and URL, and checks to see if a response exists in the cache. If
a response exists in the cache, the cache adapter then checks to make sure that the caching rules associated with the response
satisfy the request, and ensures that response still fresh. If the response is acceptable for the request any required
revalidation, then the cached response is served instead of contacting the origin server.

Vary
----

Cache keys are derived from a request method and a request URL. Multiple responses can map to the same cache key and
stored in Guzzle's underlying cache storage object. You should use the ``Vary`` HTTP header to tell the cache storage
object that the cache response must have been cached for a request that matches the headers specified in the Vary header
of the request. This allows you to have specific cache entries for the same request URL but variations in a request's
headers determine which cache entry is served. Please see the http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.44
for more information.

Cache options
-------------

There are several options you can add to requests or clients to modify the behavior of the cache plugin.

Override cache TTL
~~~~~~~~~~~~~~~~~~

You can override the number of seconds a cacheable response is stored in the cache by setting the
``cache.override_ttl`` parameter on the params object of a request:

.. code-block:: php

    // If the response to the request is cacheable, then the response will be cached for 100 seconds
    $request->getParams()->set('cache.override_ttl', 100);

If a response doesn't specify any freshness policy, it will be kept in cache for 3600 seconds by default.

Custom caching decision
~~~~~~~~~~~~~~~~~~~~~~~

If the service you are interacting with does not return caching headers or returns responses that are normally
something that would not be cached, you can set a custom ``can_cache`` object on the constructor of the CachePlugin
and provide a ``Guzzle\Plugin\Cache\CanCacheInterface`` object. You can use the
``Guzzle\Plugin\Cache\CallbackCanCacheStrategy`` to easily make a caching decision based on an HTTP request and
response.

Revalidation options
~~~~~~~~~~~~~~~~~~~~

You can change the revalidation behavior of a request using the ``cache.revalidate`` parameter. Setting this
parameter to ``never`` will ensure that a revalidation request is never sent, and the response is always served from
the origin server. Setting this parameter to ``skip`` will never revalidate and uses the response stored in the cache.

Normalizing requests for caching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the ``cache.key_filter`` parameter if you wish to strip certain query string parameters from your
request before creating a unique hash for the request. This parameter can be useful if your requests have query
string values that cause each request URL to be unique (thus preventing a cache hit). The ``cache.key_filter``
format is simply a comma separated list of query string values to remove from the URL when creating a cache key.
For example, here we are saying that the ``a`` and ``q`` query string variables should be ignored when generating a
cache key for the request:

.. code-block:: php

    $request->getParams()->set('cache.key_filter', 'a, q');

Other options
~~~~~~~~~~~~~

There are many other options available to the CachePlugin that can meet almost any caching requirement, including
custom revalidation implementations, custom cache key generators, custom caching decision strategies, and custom
cache storage objects. Take a look the constructor of ``Guzzle\Plugin\Cache\CachePlugin`` for more information.

Setting Client-wide cache settings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can specify cache settings for every request created by a client by adding cache settings to the configuration
options of a client.

.. code-block:: php

    $client = new Guzzle\Http\Client('http://www.test.com', array(
        'request.params' => array(
            'cache.override_ttl' => 3600,
            'params.cache.revalidate' => 'never'
        )
    ));

    echo $client->get('/')->getParams()->get('cache.override_ttl');
    // >>> 3600

    echo $client->get('/')->getParams()->get('cache.revalidate');
    // >>> never

Cache revalidation
------------------

If the cache plugin determines that a response to a GET request needs revalidation, a conditional GET is transferred
to the origin server. If the origin server returns a 304 response, then a response containing the merged headers of
the cached response with the new response and the entity body of the cached response is returned. Custom revalidation
strategies can be injected into a CachePlugin if needed.

Cache adapters
--------------

Guzzle doesn't try to reinvent the wheel when it comes to caching or logging. Plenty of other frameworks have
excellent solutions in place that you are probably already using in your applications. Guzzle uses adapters for
caching and logging. The cache plugin requires a cache adapter so that is can store responses in a cache. Guzzle
currently supports cache adapters for `Doctrine 2.0 <http://www.doctrine-project.org/>`_ and the
`Zend Framework <http://framework.zend.com>`_.

Doctrine cache adapter
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: php

    use Doctrine\Common\Cache\ArrayCache;
    use Guzzle\Cache\DoctrineCacheAdapter;
    use Guzzle\Plugin\Cache\CachePlugin;

    $backend = new ArrayCache();
    $adapter = new DoctrineCacheAdapter($backend);
    $cache = new CachePlugin($adapter);

Zend Framework cache adapter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: php

    use Guzzle\Cache\ZendCacheAdapter;
    use Zend\Cache\Backend\TestBackend;

    $backend = new TestBackend();
    $adapter = new ZendCacheAdapter($backend);
    $cache = new CachePlugin($adapter);
