======================
The Guzzle HTTP client
======================

Guzzle gives PHP developers complete control over HTTP requests while utilizing HTTP/1.1 best practices. Guzzle's HTTP
functionality is a robust framework built on top of the `PHP libcurl bindings <http://www.php.net/curl>`_.

The three main parts of the Guzzle HTTP client are:

+--------------+-------------------------------------------------------------------------------------------------------+
| Clients      | ``Guzzle\Http\Client`` (creates and sends requests, associates a response with a request)             |
+--------------+-------------------------------------------------------------------------------------------------------+
| Requests     | ``Guzzle\Http\Message\Request`` (requests with no body),                                              |
|              | ``Guzzle\Http\Message\EntityEnclosingRequest`` (requests with a body)                                 |
+--------------+-------------------------------------------------------------------------------------------------------+
| Responses    | ``Guzzle\Http\Message\Response``                                                                      |
+--------------+-------------------------------------------------------------------------------------------------------+

Creating a Client
-----------------

Clients create requests, send requests, and set responses on a request object. When instantiating a client object,
you can pass an optional "base URL" and optional array of configuration options. A base URL is a
:doc:`URI template <uri-templates>` that contains the URL of a remote server. When creating requests with a relative
URL, the base URL of a client will be merged into the request's URL.

.. code-block:: php

    use Guzzle\Http\Client;

    // Create a client and provide a base URL
    $client = new Client('https://api.github.com');

    $request = $client->get('/user');
    $request->setAuth('user', 'pass');
    echo $request->getUrl();
    // >>> https://api.github.com/user

    // You must send a request in order for the transfer to occur
    $response = $request->send();

    echo $response->getBody();
    // >>> {"type":"User", ...

    echo $response->getHeader('Content-Length');
    // >>> 792

    $data = $response->json();
    echo $data['type'];
    // >>> User

Base URLs
~~~~~~~~~

Notice that the URL provided to the client's ``get()`` method is relative. Relative URLs will always merge into the
base URL of the client. There are a few rules that control how the URLs are merged.

.. tip::

    Guzzle follows `RFC 3986 <http://tools.ietf.org/html/rfc3986#section-5.2>`_ when merging base URLs and
    relative URLs.

In the above example, we passed ``/user`` to the ``get()`` method of the client. This is a relative URL, so it will
merge into the base URL of the client-- resulting in the derived URL of ``https://api.github.com/users``.

``/user`` is a relative URL but uses an absolute path because it contains the leading slash. Absolute paths will
overwrite any existing path of the base URL. If an absolute path is provided (e.g. ``/path/to/something``), then the
path specified in the base URL of the client will be replaced with the absolute path, and the query string provided
by the relative URL will replace the query string of the base URL.

Omitting the leading slash and using relative paths will add to the path of the base URL of the client. So using a
client base URL of ``https://api.twitter.com/v1.1`` and creating a GET request with ``statuses/user_timeline.json``
will result in a URL of ``https://api.twitter.com/v1.1/statuses/user_timeline.json``. If a relative path and a query
string are provided, then the relative path will be appended to the base URL path, and the query string provided will
be merged into the query string of the base URL.

If an absolute URL is provided (e.g. ``http://httpbin.org/ip``), then the request will completely use the absolute URL
as-is without merging in any of the URL parts specified in the base URL.

Configuration options
~~~~~~~~~~~~~~~~~~~~~

The second argument of the client's constructor is an array of configuration data. This can include URI template data
or special options that alter the client's behavior:

+-------------------------------+-------------------------------------------------------------------------------------+
| ``request.options``           | Associative array of :ref:`Request options <request-options>` to apply to every     |
|                               | request created by the client.                                                      |
+-------------------------------+-------------------------------------------------------------------------------------+
| ``redirect.disable``          | Disable HTTP redirects for every request created by the client.                     |
+-------------------------------+-------------------------------------------------------------------------------------+
| ``curl.options``              | Associative array of cURL options to apply to every request created by the client.  |
|                               | if either the key or value of an entry in the array is a string, Guzzle will        |
|                               | attempt to find a matching defined cURL constant automatically (e.g.                |
|                               | "CURLOPT_PROXY" will be converted to the constant ``CURLOPT_PROXY``).               |
+-------------------------------+-------------------------------------------------------------------------------------+
| ``ssl.certificate_authority`` | Set to true to use the Guzzle bundled SSL certificate bundle (this is used by       |
|                               | default, 'system' to use the bundle on your system, a string pointing to a file to  |
|                               | use a specific certificate file, a string pointing to a directory to use multiple   |
|                               | certificates, or ``false`` to disable SSL validation (not recommended).             |
|                               |                                                                                     |
|                               | When using  Guzzle inside of a phar file, the bundled SSL certificate will be       |
|                               | extracted to your system's temp folder, and each time a client is created an MD5    |
|                               | check will be performed to ensure the integrity of the certificate.                 |
+-------------------------------+-------------------------------------------------------------------------------------+
| ``command.params``            | When using a ``Guzzle\Service\Client`` object, this is an associative array of      |
|                               | default options to set on each command created by the client.                       |
+-------------------------------+-------------------------------------------------------------------------------------+

Here's an example showing how to set various configuration options, including default headers to send with each request,
default query string parameters to add to each request, a default auth scheme for each request, and a proxy to use for
each request. Values can be injected into the client's base URL using variables from the configuration array.

.. code-block:: php

    use Guzzle\Http\Client;

    $client = new Client('https://api.twitter.com/{version}', array(
        'version'        => 'v1.1',
        'request.options' => array(
            'headers' => array('Foo' => 'Bar'),
            'query'   => array('testing' => '123'),
            'auth'    => array('username', 'password', 'Basic|Digest|NTLM|Any'),
            'proxy'   => 'tcp://localhost:80'
        )
    ));

Setting a custom User-Agent
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The default Guzzle User-Agent header is ``Guzzle/<Guzzle_Version> curl/<curl_version> PHP/<PHP_VERSION>``. You can
customize the User-Agent header of a client by calling the ``setUserAgent()`` method of a Client object.

.. code-block:: php

    // Completely override the default User-Agent
    $client->setUserAgent('Test/123');

    // Prepend a string to the default User-Agent
    $client->setUserAgent('Test/123', true);

Creating requests with a client
-------------------------------

A Client object exposes several methods used to create Request objects:

* Create a custom HTTP request: ``$client->createRequest($method, $uri, array $headers, $body, $options)``
* Create a GET request: ``$client->get($uri, array $headers, $options)``
* Create a HEAD request: ``$client->head($uri, array $headers, $options)``
* Create a DELETE request: ``$client->delete($uri, array $headers, $body, $options)``
* Create a POST request: ``$client->post($uri, array $headers, $postBody, $options)``
* Create a PUT request: ``$client->put($uri, array $headers, $body, $options)``
* Create a PATCH request: ``$client->patch($uri, array $headers, $body, $options)``

.. code-block:: php

    use Guzzle\Http\Client;

    $client = new Client('http://baseurl.com/api/v1');

    // Create a GET request using Relative to base URL
    // URL of the request: http://baseurl.com/api/v1/path?query=123&value=abc)
    $request = $client->get('path?query=123&value=abc');
    $response = $request->send();

    // Create HEAD request using a relative URL with an absolute path
    // URL of the request: http://baseurl.com/path?query=123&value=abc
    $request = $client->head('/path?query=123&value=abc');
    $response = $request->send();

    // Create a DELETE request using an absolute URL
    $request = $client->delete('http://www.example.com/path?query=123&value=abc');
    $response = $request->send();

    // Create a PUT request using the contents of a PHP stream as the body
    // Specify custom HTTP headers
    $request = $client->put('http://www.example.com/upload', array(
        'X-Header' => 'My Header'
    ), fopen('http://www.test.com/', 'r'));
    $response = $request->send();

    // Create a POST request and add the POST files manually
    $request = $client->post('http://localhost:8983/solr/update')
        ->addPostFiles(array('file' => '/path/to/documents.xml'));
    $response = $request->send();

    // Check if a resource supports the DELETE method
    $supportsDelete = $client->options('/path')->send()->isMethodAllowed('DELETE');
    $response = $request->send();

Client objects create Request objects using a request factory (``Guzzle\Http\Message\RequestFactoryInterface``).
You can inject a custom request factory into the Client using ``$client->setRequestFactory()``, but you can typically
rely on a Client's default request factory.

Static clients
--------------

You can use Guzzle's static client facade to more easily send simple HTTP requests.

.. code-block:: php

    // Mount the client so that you can access it at \Guzzle
    Guzzle\Http\StaticClient::mount();
    $response = Guzzle::get('http://guzzlephp.org');

Each request method of the static client (e.g. ``get()``, ``post()`, ``put()``, etc) accepts an associative array of request
options to apply to the request.

.. code-block:: php

    $response = Guzzle::post('http://test.com', array(
        'headers' => array('X-Foo' => 'Bar'),
        'body'    => array('Test' => '123'),
        'timeout' => 10
    ));

.. _request-options:

Request options
---------------

Request options can be specified when creating a request or in the ``request.options`` parameter of a client. These
options can control various aspects of a request including: headers to send, query string data, where the response
should be downloaded, proxies, auth, etc.

headers
~~~~~~~

Associative array of headers to apply to the request. When specified in the ``$options`` argument of a client creational
method (e.g. ``get()``, ``post()``, etc), the headers in the ``$options`` array will overwrite headers specified in the
``$headers`` array.

.. code-block:: php

    $request = $client->get($url, array(), array(
        'headers' => array('X-Foo' => 'Bar')
    ));

Headers can be specified on a client to add default headers to every request sent by a client.

.. code-block:: php

    $client = new Guzzle\Http\Client();

    // Set a single header using path syntax
    $client->setDefaultOption('headers/X-Foo', 'Bar');

    // Set all headers
    $client->setDefaultOption('headers', array('X-Foo' => 'Bar'));

.. note::

    In addition to setting request options when creating requests or using the ``setDefaultOption()`` method, any
    default client request option can be set using a client's config object:

    .. code-block:: php

        $client->getConfig()->setPath('request.options/headers/X-Foo', 'Bar');

query
~~~~~

Associative array of query string parameters to the request. When specified in the ``$options`` argument of a client
creational method, the query string parameters in the ``$options`` array will overwrite query string parameters
specified in the `$url`.

.. code-block:: php

    $request = $client->get($url, array(), array(
        'query' => array('abc' => '123')
    ));

Query string parameters can be specified on a client to add default query string parameters to every request sent by a
client.

.. code-block:: php

    $client = new Guzzle\Http\Client();

    // Set a single query string parameter using path syntax
    $client->setDefaultOption('query/abc', '123');

    // Set an array of default query string parameters
    $client->setDefaultOption('query', array('abc' => '123'));

body
~~~~

Sets the body of a request. The value supplied to the body option can be a ``Guzzle\Http\EntityBodyInterface``, string,
fopen resource, or array when sending POST requests. When a ``body`` request option is supplied, the option value will
overwrite the ``$body`` argument of a client creational method.

auth
~~~~

Specifies and array of HTTP authorization parameters parameters to use with the request. The array must contain the
username in index [0], the password in index [1], and can optionally contain the authentication type in index [2].
The available authentication types are: "Basic" (default), "Digest", "NTLM", or "Any".

.. code-block:: php

    $request = $client->get($url, array(), array(
        'auth' => array('username', 'password', 'Digest')
    ));

    // You can add auth headers to every request of a client
    $client->setDefaultOption('auth', array('username', 'password', 'Digest'));

cookies
~~~~~~~

Specifies an associative array of cookies to add to the request.

allow_redirects
~~~~~~~~~~~~~~~

Specifies whether or not the request should follow redirects. Requests will follow redirects by default. Set
``allow_redirects`` to ``false`` to disable redirects.

save_to
~~~~~~~

The ``save_to`` option specifies where the body of a response is downloaded. You can pass the path to a file, an fopen
resource, or a ``Guzzle\Http\EntityBodyInterface`` object.

See :ref:`Changing where a response is downloaded <request-set-response-body>` for more information on setting the
`save_to` option.

events
~~~~~~

The `events` option makes it easy to attach listeners to the various events emitted by a request object. The `events`
options must be an associative array mapping an event name to a Closure or array the contains a Closure and the
priority of the event.

.. code-block:: php

    $request = $client->get($url, array(), array(
        'events' => array(
            'request.before_send' => function (\Guzzle\Common\Event $e) {
                echo 'About to send ' . $e['request'];
            }
        )
    ));

    // Using the static client:
    Guzzle::get($url, array(
        'events' => array(
            'request.before_send' => function (\Guzzle\Common\Event $e) {
                echo 'About to send ' . $e['request'];
            }
        )
    ));

plugins
~~~~~~~

The `plugins` options makes it easy to attach an array of plugins to a request.

.. code-block:: php

    // Using the static client:
    Guzzle::get($url, array(
        'plugins' => array(
            new Guzzle\Plugin\Cache\CachePlugin(),
            new Guzzle\Plugin\Cookie\CookiePlugin()
        )
    ));

exceptions
~~~~~~~~~~

The `exceptions` option can be used to disable throwing exceptions for unsuccessful HTTP response codes
(e.g. 404, 500, etc). Set `exceptions` to false to not throw exceptions.

params
~~~~~~

The `params` options can be used to specify an associative array of data parameters to add to a request.  Note that
these are not query string parameters.

timeout / connect_timeout
~~~~~~~~~~~~~~~~~~~~~~~~~

You can specify the maximum number of seconds to allow for an entire transfer to take place before timing out using
the `timeout` request option. You can specify the maximum number of seconds to wait while trying to connect using the
`connect_timeout` request option. Set either of these options to 0 to wait indefinitely.

.. code-block:: php

    $request = $client->get('http://www.example.com', array(), array(
        'timeout'         => 20,
        'connect_timeout' => 1.5
    ));

verify
~~~~~~

Set to true to enable SSL certificate validation (the default), false to disable SSL certificate validation, or supply
the path to a CA bundle to enable verification using a custom certificate.

cert
~~~~

The `cert` option lets you specify a PEM formatted SSL client certificate to use with servers that require one. If the
certificate requires a password, provide an array with the password as the second item.

This would typically be used in conjunction with the `ssl_key` option.

.. code-block:: php

    $request = $client->get('https://www.example.com', array(), array(
        'cert' => '/etc/pki/client_certificate.pem'
    )

    $request = $client->get('https://www.example.com', array(), array(
        'cert' => array('/etc/pki/client_certificate.pem', 's3cr3tp455w0rd')
    )

ssl_key
~~~~~~~

The `ssl_key` option lets you specify a file containing your PEM formatted private key, optionally protected by a password.
Note: your password is sensitive, keep the PHP script containing it safe.

This would typically be used in conjunction with the `cert` option.

.. code-block:: php

    $request = $client->get('https://www.example.com', array(), array(
        'ssl_key' => '/etc/pki/private_key.pem'
    )

    $request = $client->get('https://www.example.com', array(), array(
        'ssl_key' => array('/etc/pki/private_key.pem', 's3cr3tp455w0rd')
    )

proxy
~~~~~

The `proxy` option is used to specify an HTTP proxy (e.g. `http://username:password@192.168.16.1:10`).

debug
~~~~~

The `debug` option is used to show verbose cURL output for a transfer.

stream
~~~~~~

When using a static client, you can set the `stream` option to true to return a `Guzzle\Stream\Stream` object that can
be used to pull data from a stream as needed (rather than have cURL download the entire contents of a response to a
stream all at once).

.. code-block:: php

    $stream = Guzzle::get('http://guzzlephp.org', array('stream' => true));
    while (!$stream->feof()) {
        echo $stream->readLine();
    }

Sending requests
----------------

Requests can be sent by calling the ``send()`` method of a Request object, but you can also send requests using the
``send()`` method of a Client.

.. code-block:: php

    $request = $client->get('http://www.amazon.com');
    $response = $client->send($request);

Sending requests in parallel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Client's ``send()`` method accept a single ``Guzzle\Http\Message\RequestInterface`` object or an array of
RequestInterface objects. When an array is specified, the requests will be sent in parallel.

Sending many HTTP requests serially (one at a time) can cause an unnecessary delay in a script's execution. Each
request must complete before a subsequent request can be sent. By sending requests in parallel, a pool of HTTP
requests can complete at the speed of the slowest request in the pool, significantly reducing the amount of time
needed to execute multiple HTTP requests. Guzzle provides a wrapper for the curl_multi functions in PHP.

Here's an example of sending three requests in parallel using a client object:

.. code-block:: php

    use Guzzle\Common\Exception\MultiTransferException;

    try {
        $responses = $client->send(array(
            $client->get('http://www.google.com/'),
            $client->head('http://www.google.com/'),
            $client->get('https://www.github.com/')
        ));
    } catch (MultiTransferException $e) {

        echo "The following exceptions were encountered:\n";
        foreach ($e as $exception) {
            echo $exception->getMessage() . "\n";
        }

        echo "The following requests failed:\n";
        foreach ($e->getFailedRequests() as $request) {
            echo $request . "\n\n";
        }

        echo "The following requests succeeded:\n";
        foreach ($e->getSuccessfulRequests() as $request) {
            echo $request . "\n\n";
        }
    }

If the requests succeed, an array of ``Guzzle\Http\Message\Response`` objects are returned. A single request failure
will not cause the entire pool of requests to fail. Any exceptions thrown while transferring a pool of requests will
be aggregated into a ``Guzzle\Common\Exception\MultiTransferException`` exception.

Plugins and events
------------------

Guzzle provides easy to use request plugins that add behavior to requests based on signal slot event notifications
powered by the
`Symfony2 Event Dispatcher component <http://symfony.com/doc/2.0/components/event_dispatcher/introduction.html>`_. Any
event listener or subscriber attached to a Client object will automatically be attached to each request created by the
client.

Using the same cookie session for each request
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Attach a ``Guzzle\Plugin\Cookie\CookiePlugin`` to a client which will in turn add support for cookies to every request
created by a client, and each request will use the same cookie session:

.. code-block:: php

    use Guzzle\Plugin\Cookie\CookiePlugin;
    use Guzzle\Plugin\Cookie\CookieJar\ArrayCookieJar;

    // Create a new cookie plugin
    $cookiePlugin = new CookiePlugin(new ArrayCookieJar());

    // Add the cookie plugin to the client
    $client->addSubscriber($cookiePlugin);

.. _client-events:

Events emitted from a client
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A ``Guzzle\Http\Client`` object emits the following events:

+------------------------------+--------------------------------------------+------------------------------------------+
| Event name                   | Description                                | Event data                               |
+==============================+============================================+==========================================+
| client.create_request        | Called when a client creates a request     | * client: The client                     |
|                              |                                            | * request: The created request           |
+------------------------------+--------------------------------------------+------------------------------------------+

.. code-block:: php

    use Guzzle\Common\Event;
    use Guzzle\Http\Client;

    $client = new Client();

    // Add a listener that will echo out requests as they are created
    $client->getEventDispatcher()->addListener('client.create_request', function (Event $e) {
        echo 'Client object: ' . spl_object_hash($e['client']) . "\n";
        echo "Request object: {$e['request']}\n";
    });
