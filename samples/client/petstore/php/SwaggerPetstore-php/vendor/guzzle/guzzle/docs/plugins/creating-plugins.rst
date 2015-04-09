================
Creating plugins
================

.. highlight:: php

Guzzle is extremely extensible because of the behavioral modifications that can be added to requests, clients, and
commands using an event system. Before and after the majority of actions are taken in the library, an event is emitted
with the name of the event and context surrounding the event. Observers can subscribe to a subject and modify the
subject based on the events received. Guzzle's event system utilizes the Symfony2 EventDispatcher and is the backbone
of its plugin architecture.

Overview
--------

Plugins must implement the ``Symfony\Component\EventDispatcher\EventSubscriberInterface`` interface. The
``EventSubscriberInterface`` requires that your class implements a static method, ``getSubscribedEvents()``, that
returns an associative array mapping events to methods on the object. See the
`Symfony2 documentation <http://symfony.com/doc/2.0/book/internals.html#the-event-dispatcher>`_ for more information.

Plugins can be attached to any subject, or object in Guzzle that implements that
``Guzzle\Common\HasDispatcherInterface``.

Subscribing to a subject
~~~~~~~~~~~~~~~~~~~~~~~~

You can subscribe an instantiated observer to an event by calling ``addSubscriber`` on a subject.

.. code-block:: php

    $testPlugin = new TestPlugin();
    $client->addSubscriber($testPlugin);

You can also subscribe to only specific events using a closure::

    $client->getEventDispatcher()->addListener('request.create', function(Event $event) {
        echo $event->getName();
        echo $event['request'];
    });

``Guzzle\Common\Event`` objects are passed to notified functions. The Event object has a ``getName()`` method which
return the name of the emitted event and may contain contextual information that can be accessed like an array.

Knowing what events to listen to
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any class that implements the ``Guzzle\Common\HasDispatcherInterface`` must implement a static method,
``getAllEvents()``, that returns an array of the events that are emitted from the object.  You can browse the source
to see each event, or you can call the static method directly in your code to get a list of available events.

Event hooks
-----------

* :ref:`client-events`
* :ref:`service-client-events`
* :ref:`request-events`
* ``Guzzle\Http\Curl\CurlMulti``:
* :ref:`service-builder-events`

Examples of the event system
----------------------------

Simple Echo plugin
~~~~~~~~~~~~~~~~~~

This simple plugin prints a string containing the request that is about to be sent by listening to the
``request.before_send`` event::

    use Symfony\Component\EventDispatcher\EventSubscriberInterface;

    class EchoPlugin implements EventSubscriberInterface
    {
        public static function getSubscribedEvents()
        {
            return array('request.before_send' => 'onBeforeSend');
        }

        public function onBeforeSend(Guzzle\Common\Event $event)
        {
            echo 'About to send a request: ' . $event['request'] . "\n";
        }
    }

    $client = new Guzzle\Service\Client('http://www.test.com/');

    // Create the plugin and add it as an event subscriber
    $plugin = new EchoPlugin();
    $client->addSubscriber($plugin);

    // Send a request and notice that the request is printed to the screen
    $client->get('/')->send();

Running the above code will print a string containing the HTTP request that is about to be sent.
