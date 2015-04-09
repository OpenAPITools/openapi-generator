====================
Backoff retry plugin
====================

The ``Guzzle\Plugin\Backoff\BackoffPlugin`` automatically retries failed HTTP requests using custom backoff strategies:

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Plugin\Backoff\BackoffPlugin;

    $client = new Client('http://www.test.com/');
    // Use a static factory method to get a backoff plugin using the exponential backoff strategy
    $backoffPlugin = BackoffPlugin::getExponentialBackoff();

    // Add the backoff plugin to the client object
    $client->addSubscriber($backoffPlugin);

The BackoffPlugin's constructor accepts a ``Guzzle\Plugin\Backoff\BackoffStrategyInterface`` object that is used to
determine when a retry should be issued and how long to delay between retries. The above code example shows how to
attach a BackoffPlugin to a client that is pre-configured to retry failed 500 and 503 responses using truncated
exponential backoff (emulating the behavior of Guzzle 2's ExponentialBackoffPlugin).
