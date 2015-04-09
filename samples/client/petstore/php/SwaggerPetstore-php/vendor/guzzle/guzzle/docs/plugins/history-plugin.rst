==============
History plugin
==============

The history plugin tracks all of the requests and responses sent through a request or client. This plugin can be
useful for crawling or unit testing. By default, the history plugin stores up to 10 requests and responses.

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Plugin\History\HistoryPlugin;

    $client = new Client('http://www.test.com/');

    // Add the history plugin to the client object
    $history = new HistoryPlugin();
    $history->setLimit(5);
    $client->addSubscriber($history);

    $client->get('http://www.yahoo.com/')->send();

    echo $history->getLastRequest();
    echo $history->getLastResponse();
    echo count($history);
