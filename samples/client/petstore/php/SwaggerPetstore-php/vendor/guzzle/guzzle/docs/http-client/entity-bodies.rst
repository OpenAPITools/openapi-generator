===========================
Request and response bodies
===========================

`Entity body <http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html>`_ is the term used for the body of an HTTP
message. The entity body of requests and responses is inherently a
`PHP stream <http://php.net/manual/en/book.stream.php>`_ in Guzzle. The body of the request can be either a string or
a PHP stream which are converted into a ``Guzzle\Http\EntityBody`` object using its factory method. When using a
string, the entity body is stored in a `temp PHP stream <http://www.php.net/manual/en/wrappers.php.php>`_. The use of
temp PHP streams helps to protect your application from running out of memory when sending or receiving large entity
bodies in your messages. When more than 2MB of data is stored in a temp stream, it automatically stores the data on
disk rather than in memory.

EntityBody objects provide a great deal of functionality: compression, decompression, calculate the Content-MD5,
calculate the Content-Length (when the resource is repeatable), guessing the Content-Type, and more. Guzzle doesn't
need to load an entire entity body into a string when sending or retrieving data; entity bodies are streamed when
being uploaded and downloaded.

Here's an example of gzip compressing a text file then sending the file to a URL:

.. code-block:: php

    use Guzzle\Http\EntityBody;

    $body = EntityBody::factory(fopen('/path/to/file.txt', 'r+'));
    echo $body->read(1024);
    $body->seek(0, SEEK_END);
    $body->write('foo');
    echo $body->ftell();
    $body->rewind();

    // Send a request using the body
    $response = $client->put('http://localhost:8080/uploads', null, $body)->send();

The body of the request can be specified in the ``Client::put()`` or ``Client::post()``  method, or, you can specify
the body of the request by calling the ``setBody()`` method of any
``Guzzle\Http\Message\EntityEnclosingRequestInterface`` object.

Compression
-----------

You can compress the contents of an EntityBody object using the ``compress()`` method. The compress method accepts a
filter that must match to one of the supported
`PHP stream filters <http://www.php.net/manual/en/filters.compression.php>`_ on your system (e.g. `zlib.deflate`,
``bzip2.compress``, etc). Compressing an entity body will stream the entire entity body through a stream compression
filter into a temporary PHP stream. You can uncompress an entity body using the ``uncompress()`` method and passing
the PHP stream filter to use when decompressing the stream (e.g. ``zlib.inflate``).

.. code-block:: php

    use Guzzle\Http\EntityBody;

    $body = EntityBody::factory(fopen('/tmp/test.txt', 'r+'));
    echo $body->getSize();
    // >>> 1048576

    // Compress using the default zlib.deflate filter
    $body->compress();
    echo $body->getSize();
    // >>> 314572

    // Decompress the stream
    $body->uncompress();
    echo $body->getSize();
    // >>> 1048576

Decorators
----------

Guzzle provides several EntityBody decorators that can be used to add functionality to an EntityBody at runtime.

IoEmittingEntityBody
~~~~~~~~~~~~~~~~~~~~

This decorator will emit events when data is read from a stream or written to a stream. Add an event subscriber to the
entity body's ``body.read`` or ``body.write`` methods to receive notifications when data data is transferred.

.. code-block:: php

    use Guzzle\Common\Event;
    use Guzzle\Http\EntityBody;
    use Guzzle\Http\IoEmittingEntityBody;

    $original = EntityBody::factory(fopen('/tmp/test.txt', 'r+'));
    $body = new IoEmittingEntityBody($original);

    // Listen for read events
    $body->getEventDispatcher()->addListener('body.read', function (Event $e) {
        // Grab data from the event
        $entityBody = $e['body'];
        // Amount of data retrieved from the body
        $lengthOfData = $e['length'];
        // The actual data that was read
        $data = $e['read'];
    });

    // Listen for write events
    $body->getEventDispatcher()->addListener('body.write', function (Event $e) {
        // Grab data from the event
        $entityBody = $e['body'];
        // The data that was written
        $data = $e['write'];
        // The actual amount of data that was written
        $data = $e['read'];
    });

ReadLimitEntityBody
~~~~~~~~~~~~~~~~~~~

The ReadLimitEntityBody decorator can be used to transfer a subset or slice of an existing EntityBody object. This can
be useful for breaking a large file into smaller pieces to be sent in chunks (e.g. Amazon S3's multipart upload API).

.. code-block:: php

    use Guzzle\Http\EntityBody;
    use Guzzle\Http\ReadLimitEntityBody;

    $original = EntityBody::factory(fopen('/tmp/test.txt', 'r+'));
    echo $original->getSize();
    // >>> 1048576

    // Limit the size of the body to 1024 bytes and start reading from byte 2048
    $body = new ReadLimitEntityBody($original, 1024, 2048);
    echo $body->getSize();
    // >>> 1024
    echo $body->ftell();
    // >>> 0

CachingEntityBody
~~~~~~~~~~~~~~~~~

The CachingEntityBody decorator is used to allow seeking over previously read bytes on non-seekable read streams. This
can be useful when transferring a non-seekable entity body fails due to needing to rewind the stream (for example,
resulting from a redirect). Data that is read from the remote stream will be buffered in a PHP temp stream so that
previously read bytes are cached first in memory, then on disk.

.. code-block:: php

    use Guzzle\Http\EntityBody;
    use Guzzle\Http\CachingEntityBody;

    $original = EntityBody::factory(fopen('http://www.google.com', 'r'));
    $body = new CachingEntityBody($original);

    $body->read(1024);
    echo $body->ftell();
    // >>> 1024

    $body->seek(0);
    echo $body->ftell();
    // >>> 0
