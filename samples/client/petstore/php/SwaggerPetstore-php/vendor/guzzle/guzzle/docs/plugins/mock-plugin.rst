===========
Mock plugin
===========

The mock plugin is useful for testing Guzzle clients. The mock plugin allows you to queue an array of responses that
will satisfy requests sent from a client by consuming the request queue in FIFO order.

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Plugin\Mock\MockPlugin;
    use Guzzle\Http\Message\Response;

    $client = new Client('http://www.test.com/');

    $mock = new MockPlugin();
    $mock->addResponse(new Response(200))
         ->addResponse(new Response(404));

    // Add the mock plugin to the client object
    $client->addSubscriber($mock);

    // The following request will receive a 200 response from the plugin
    $client->get('http://www.example.com/')->send();

    // The following request will receive a 404 response from the plugin
    $client->get('http://www.test.com/')->send();
