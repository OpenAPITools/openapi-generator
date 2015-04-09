======================
Plugin system overview
======================

The workflow of sending a request and parsing a response is driven by Guzzle's event system, which is powered by the
`Symfony2 Event Dispatcher component <http://symfony.com/doc/current/components/event_dispatcher/introduction.html>`_.

Any object in Guzzle that emits events will implement the ``Guzzle\Common\HasEventDispatcher`` interface. You can add
event subscribers directly to these objects using the ``addSubscriber()`` method, or you can grab the
``Symfony\Component\EventDispatcher\EventDispatcher`` object owned by the object using ``getEventDispatcher()`` and
add a listener or event subscriber.

Adding event subscribers to clients
-----------------------------------

Any event subscriber or event listener attached to the EventDispatcher of a ``Guzzle\Http\Client`` or
``Guzzle\Service\Client`` object will automatically be attached to all request objects created by the client. This
allows you to attach, for example, a HistoryPlugin to a client object, and from that point on, every request sent
through that client will utilize the HistoryPlugin.

.. code-block:: php

    use Guzzle\Plugin\History\HistoryPlugin;
    use Guzzle\Service\Client;

    $client = new Client();

    // Create a history plugin and attach it to the client
    $history = new HistoryPlugin();
    $client->addSubscriber($history);

    // Create and send a request. This request will also utilize the HistoryPlugin
    $client->get('http://httpbin.org')->send();

    // Echo out the last sent request by the client
    echo $history->getLastRequest();

.. tip::

    :doc:`Create event subscribers <creating-plugins>`, or *plugins*, to implement reusable logic that can be
    shared across clients. Event subscribers are also easier to test than anonymous functions.

Pre-Built plugins
-----------------

Guzzle provides easy to use request plugins that add behavior to requests based on signal slot event notifications
powered by the Symfony2 Event Dispatcher component.

* :doc:`async-plugin`
* :doc:`backoff-plugin`
* :doc:`cache-plugin`
* :doc:`cookie-plugin`
* :doc:`curl-auth-plugin`
* :doc:`history-plugin`
* :doc:`log-plugin`
* :doc:`md5-validator-plugin`
* :doc:`mock-plugin`
* :doc:`oauth-plugin`

