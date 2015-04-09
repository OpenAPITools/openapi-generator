======================
Using Response objects
======================

Sending a request will return a ``Guzzle\Http\Message\Response`` object. You can view the raw  HTTP response message by
casting the Response object to a string. Casting the response to a string will return the entity body of the response
as a string too, so this might be an expensive operation if the entity body is stored in a file or network stream. If
you only want to see the response headers, you can call ``getRawHeaders()``.

Response status line
--------------------

The different parts of a response's `status line <http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1>`_
(the first line of the response HTTP message) are easily retrievable.

.. code-block:: php

    $response = $client->get('http://www.amazon.com')->send();

    echo $response->getStatusCode();      // >>> 200
    echo $response->getReasonPhrase();    // >>> OK
    echo $response->getProtocol();        // >>> HTTP
    echo $response->getProtocolVersion(); // >>> 1.1

You can determine the type of the response using several helper methods:

.. code-block:: php

    $response->isSuccessful(); // true
    $response->isInformational();
    $response->isRedirect();
    $response->isClientError();
    $response->isServerError();

Response headers
----------------

The Response object contains helper methods for retrieving common response headers. These helper methods normalize the
variations of HTTP response headers.

.. code-block:: php

    $response->getCacheControl();
    $response->getContentType();
    $response->getContentLength();
    $response->getContentEncoding();
    $response->getContentMd5();
    $response->getEtag();
    // etc... There are methods for every known response header

You can interact with the Response headers using the same exact methods used to interact with Request headers. See
:ref:`http-message-headers` for more information.

.. code-block:: php

    echo $response->getHeader('Content-Type');
    echo $response->getHeader('Content-Length');
    echo $response->getHeaders()['Content-Type']; // PHP 5.4

Response body
-------------

The entity body object of a response can be retrieved by calling ``$response->getBody()``. The response EntityBody can
be cast to a string, or you can pass ``true`` to this method to retrieve the body as a string.

.. code-block:: php

    $request = $client->get('http://www.amazon.com');
    $response = $request->send();
    echo $response->getBody();

See :doc:`/http-client/entity-bodies` for more information on entity bodies.

JSON Responses
~~~~~~~~~~~~~~

You can easily parse and use a JSON response as an array using the ``json()`` method of a response. This method will
always return an array if the response is valid JSON or if the response body is empty. You will get an exception if you
call this method and the response is not valid JSON.

.. code-block:: php

    $data = $response->json();
    echo gettype($data);
    // >>> array

XML Responses
~~~~~~~~~~~~~

You can easily parse and use a XML response as SimpleXMLElement object using the ``xml()`` method of a response. This
method will always return a SimpleXMLElement object if the response is valid XML or if the response body is empty. You
will get an exception if you call this method and the response is not valid XML.

.. code-block:: php

    $xml = $response->xml();
    echo $xml->foo;
    // >>> Bar!

Streaming responses
-------------------

Some web services provide streaming APIs that allow a client to keep a HTTP request open for an extended period of
time while polling and reading. Guzzle provides a simple way to convert HTTP request messages into
``Guzzle\Stream\Stream`` objects so that you can send the initial headers of a request, read the response headers, and
pull in the response body manually as needed.

Here's an example using the Twitter Streaming API to track the keyword "bieber":

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Stream\PhpStreamRequestFactory;

    $client = new Client('https://stream.twitter.com/1');

    $request = $client->post('statuses/filter.json', null, array(
        'track' => 'bieber'
    ));

    $request->setAuth('myusername', 'mypassword');

    $factory = new PhpStreamRequestFactory();
    $stream = $factory->fromRequest($request);

    // Read until the stream is closed
    while (!$stream->feof()) {
        // Read a line from the stream
        $line = $stream->readLine();
        // JSON decode the line of data
        $data = json_decode($line, true);
    }

You can use the ``stream`` request option when using a static client to more easily create a streaming response.

.. code-block:: php

    $stream = Guzzle::get('http://guzzlephp.org', array('stream' => true));
    while (!$stream->feof()) {
        echo $stream->readLine();
    }
