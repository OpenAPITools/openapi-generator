===========================
Unit Testing Guzzle clients
===========================

Guzzle provides several tools that will enable you to easily unit test your web service clients.

* PHPUnit integration
* Mock responses
* node.js web server for integration testing

PHPUnit integration
-------------------

Guzzle is unit tested using `PHPUnit <http://www.phpunit.de/>`_.  Your web service client's unit tests should extend
``Guzzle\Tests\GuzzleTestCase`` so that you can take advantage of some of the built in helpers.

In order to unit test your client, a developer would need to copy phpunit.xml.dist to phpunit.xml and make any needed
modifications.  As a best practice and security measure for you and your contributors, it is recommended to add an
ignore statement to your SCM so that phpunit.xml is ignored.

Bootstrapping
~~~~~~~~~~~~~

Your web service client should have a tests/ folder that contains a bootstrap.php file. The bootstrap.php file
responsible for autoloading and configuring a ``Guzzle\Service\Builder\ServiceBuilder`` that is used throughout your
unit tests for loading a configured client. You can add custom parameters to your phpunit.xml file that expects users
to provide the path to their configuration data.

.. code-block:: php

    Guzzle\Tests\GuzzleTestCase::setServiceBuilder(Aws\Common\Aws::factory($_SERVER['CONFIG']));

    Guzzle\Tests\GuzzleTestCase::setServiceBuilder(Guzzle\Service\Builder\ServiceBuilder::factory(array(
        'test.unfuddle' => array(
            'class' => 'Guzzle.Unfuddle.UnfuddleClient',
            'params' => array(
                'username' => 'test_user',
                'password' => '****',
                'subdomain' => 'test'
            )
        )
    )));

The above code registers a service builder that can be used throughout your unit tests.  You would then be able to
retrieve an instantiated and configured Unfuddle client by calling ``$this->getServiceBuilder()->get('test.unfuddle)``.
The above code assumes that ``$_SERVER['CONFIG']`` contains the path to a file that stores service description
configuration.

Unit testing remote APIs
------------------------

Mock responses
~~~~~~~~~~~~~~

One of the benefits of unit testing is the ability to quickly determine if there are errors in your code.  If your
unit tests run slowly, then they become tedious and will likely be run less frequently.  Guzzle's philosophy on unit
testing web service clients is that no network access should be required to run the unit tests.  This means that
responses are served from mock responses or local servers.  By adhering to this principle, tests will run much faster
and will not require an external resource to be available.  The problem with this approach is that your mock responses
must first be gathered and then subsequently updated each time the remote API changes.

Integration testing over the internet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can perform integration testing with a web service over the internet by making calls directly to the service. If
the web service you are requesting uses a complex signing algorithm or some other specific implementation, then you
may want to include at least one actual network test that can be run specifically through the command line using
`PHPUnit group annotations <http://www.phpunit.de/manual/current/en/appendixes.annotations.html#appendixes.annotations.group>`_.

@group internet annotation
^^^^^^^^^^^^^^^^^^^^^^^^^^

When creating tests that require an internet connection, it is recommended that you add ``@group internet`` annotations
to your unit tests to specify which tests require network connectivity.

You can then `run PHPUnit tests <http://www.phpunit.de/manual/current/en/textui.html>`_ that exclude the @internet
group by running ``phpunit --exclude-group internet``.

API credentials
^^^^^^^^^^^^^^^

If API  credentials are required to run your integration tests, you must add ``<php>`` parameters to your
phpunit.xml.dist file and extract these parameters in your bootstrap.php file.

.. code-block:: xml

    <?xml version="1.0" encoding="UTF-8"?>
    <phpunit bootstrap="./tests/bootstrap.php" colors="true">
        <php>
            <!-- Specify the path to a service configuration file -->
            <server name="CONFIG" value="test_services.json" />
            <!-- Or, specify each require parameter individually -->
            <server name="API_USER" value="change_me" />
            <server name="API_PASSWORD" value="****" />
        </php>
        <testsuites>
            <testsuite name="guzzle-service">
                <directory suffix="Test.php">./Tests</directory>
            </testsuite>
        </testsuites>
    </phpunit>

You can then extract the ``server`` variables in your bootstrap.php file by grabbing them from the ``$_SERVER``
superglobal: ``$apiUser = $_SERVER['API_USER'];``

Further reading
^^^^^^^^^^^^^^^

A good discussion on the topic of testing remote APIs can be found in Sebastian Bergmann's
`Real-World Solutions for Developing High-Quality PHP Frameworks and Applications <http://www.amazon.com/dp/0470872497>`_.

Queueing Mock responses
-----------------------

Mock responses can be used to test if requests are being generated correctly and responses and handled correctly by
your client.  Mock responses can be queued up for a client using the ``$this->setMockResponse($client, $path)`` method
of your test class.  Pass the client you are adding mock responses to and a single path or array of paths to mock
response files relative to the ``/tests/mock/ folder``.  This will queue one or more mock responses for your client by
creating a simple observer on the client.  Mock response files must contain a full HTTP response message:

.. code-block:: none

    HTTP/1.1 200 OK
    Date: Wed, 25 Nov 2009 12:00:00 GMT
    Connection: close
    Server: AmazonS3
    Content-Type: application/xml

    <?xml version="1.0" encoding="UTF-8"?>
    <LocationConstraint xmlns="http://s3.amazonaws.com/doc/2006-03-01/">EU</LocationConstraint>

After queuing mock responses for a client, you can get an array of the requests that were sent by the client that
were issued a mock response by calling ``$this->getMockedRequests()``.

You can also use the ``Guzzle\Plugin\Mock\MockPlugin`` object directly with your clients.

.. code-block:: php

    $plugin = new Guzzle\Plugin\Mock\MockPlugin();
    $plugin->addResponse(new Guzzle\Http\Message\Response(200));
    $client = new Guzzle\Http\Client();
    $client->addSubscriber($plugin);

    // The following request will get the mock response from the plugin in FIFO order
    $request = $client->get('http://www.test.com/');
    $request->send();

    // The MockPlugin maintains a list of requests that were mocked
    $this->assertContainsOnly($request, $plugin->getReceivedRequests());

node.js web server for integration testing
------------------------------------------

Using mock responses is usually enough when testing a web service client.  If your client needs to add custom cURL
options to requests, then you should use the node.js test web server to ensure that your HTTP request message is
being created correctly.

Guzzle is based around PHP's libcurl bindings.  cURL sometimes modifies an HTTP request message based on
``CURLOPT_*`` options.  Headers that are added to your request by cURL will not be accounted for if you inject mock
responses into your tests.  Additionally, some request entity bodies cannot be loaded by the client before transmitting
it to the sever (for example, when using a client as a sort of proxy and streaming content from a remote server). You
might also need to inspect the entity body of a ``multipart/form-data`` POST request.

.. note::

    You can skip all of the tests that require the node.js test web server by excluding the ``server`` group:
    ``phpunit --exclude-group server``

Using the test server
~~~~~~~~~~~~~~~~~~~~~

The node.js test server receives requests and returns queued responses.  The test server exposes a simple API that is
used to enqueue responses and inspect the requests that it has received.

Retrieve the server object by calling ``$this->getServer()``.  If the node.js server is not running, it will be
started as a forked process and an object that interfaces with the server will be returned.  (note: stopping the
server is handled internally by Guzzle.)

You can queue an HTTP response or an array of responses by calling ``$this->getServer()->enqueue()``:

.. code-block:: php

    $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");

The above code queues a single 200 response with an empty body.  Responses are queued using a FIFO order; this
response will be returned by the server when it receives the first request and then removed from the queue. If a
request is received by a server with no queued responses, an exception will be thrown in your unit test.

You can inspect the requests that the server has retrieved by calling ``$this->getServer()->getReceivedRequests()``.
This method accepts an optional ``$hydrate`` parameter that specifies if you are retrieving an array of string HTTP
requests or an array of ``Guzzle\Http\RequestInterface`` subclassed objects.  "Hydrating" the requests will allow
greater flexibility in your unit tests so that you can  easily assert the state of the various parts of a request.

You will need to modify the base_url of your web service client in order to use it against the test server.

.. code-block:: php

    $client = $this->getServiceBuilder()->get('my_client');
    $client->setBaseUrl($this->getServer()->getUrl());

After running the above code, all calls made from the ``$client`` object will be sent to the test web server.
