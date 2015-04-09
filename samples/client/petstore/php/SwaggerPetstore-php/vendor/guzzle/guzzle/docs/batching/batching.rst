========
Batching
========

Guzzle provides a fairly generic and very customizable batching framework that allows developers to efficiently
transfer requests in parallel.

Sending requests and commands in parallel
-----------------------------------------

You can send HTTP requests in parallel by passing an array of ``Guzzle\Http\Message\RequestInterface`` objects to
``Guzzle\Http\Client::send()``:

.. code-block:: php

    $responses = $client->send(array(
        $client->get('http://www.example.com/foo'),
        $client->get('http://www.example.com/baz')
        $client->get('http://www.example.com/bar')
    ));

You can send commands in parallel by passing an array of ``Guzzle\Service\Command\CommandInterface`` objects
``Guzzle\Service\Client::execute()``:

.. code-block:: php

    $commands = $client->execute(array(
        $client->getCommand('foo'),
        $client->getCommand('baz'),
        $client->getCommand('bar')
    ));

These approaches work well for most use-cases.  When you need more control over the requests that are sent in
parallel or you need to send a large number of requests, you need to use the functionality provided in the
``Guzzle\Batch`` namespace.

Batching overview
-----------------

The batch object, ``Guzzle\Batch\Batch``, is a queue.  You add requests to the queue until you are ready to transfer
all of the requests.  In order to efficiently transfer the items in the queue, the batch object delegates the
responsibility of dividing the queue into manageable parts to a divisor (``Guzzle\Batch\BatchDivisorInterface``).
The batch object then iterates over each array of items created by the divisor and sends them to the batch object's
``Guzzle\Batch\BatchTransferInterface``.

.. code-block:: php

    use Guzzle\Batch\Batch;
    use Guzzle\Http\BatchRequestTransfer;

    // BatchRequestTransfer acts as both the divisor and transfer strategy
    $transferStrategy = new BatchRequestTransfer(10);
    $divisorStrategy = $transferStrategy;

    $batch = new Batch($transferStrategy, $divisorStrategy);

    // Add some requests to the batch queue
    $batch->add($request1)
        ->add($request2)
        ->add($request3);

    // Flush the queue and retrieve the flushed items
    $arrayOfTransferredRequests = $batch->flush();

.. note::

    You might find that your transfer strategy will need to act as both the divisor and transfer strategy.

Using the BatchBuilder
----------------------

The ``Guzzle\Batch\BatchBuilder`` makes it easier to create batch objects.  The batch builder also provides an easier
way to add additional behaviors to your batch object.

Transferring requests
~~~~~~~~~~~~~~~~~~~~~

The ``Guzzle\Http\BatchRequestTransfer`` class efficiently transfers HTTP requests in parallel by grouping batches of
requests by the curl_multi handle that is used to transfer the requests.

.. code-block:: php

    use Guzzle\Batch\BatchBuilder;

    $batch = BatchBuilder::factory()
        ->transferRequests(10)
        ->build();

Transferring commands
~~~~~~~~~~~~~~~~~~~~~

The ``Guzzle\Service\Command\BatchCommandTransfer`` class efficiently transfers service commands by grouping commands
by the client that is used to transfer them.  You can add commands to a batch object that are transferred by different
clients, and the batch will handle the rest.

.. code-block:: php

    use Guzzle\Batch\BatchBuilder;

    $batch = BatchBuilder::factory()
        ->transferCommands(10)
        ->build();

    $batch->add($client->getCommand('foo'))
        ->add($client->getCommand('baz'))
        ->add($client->getCommand('bar'));

    $commands = $batch->flush();

Batch behaviors
---------------

You can add various behaviors to your batch that allow for more customizable transfers.

Automatically flushing a queue
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the ``Guzzle\Batch\FlushingBatch`` decorator when you want to pump a large number of items into a batch queue and
have the queue automatically flush when the size of the queue reaches a certain threshold.

.. code-block:: php

    use Guzzle\Batch\BatchBuilder;

    $batch = BatchBuilder::factory()
        ->transferRequests(10)
        ->autoFlushAt(10)
        ->build();

Batch builder method: ``autoFlushAt($threshold)``

Notifying on flush
~~~~~~~~~~~~~~~~~~

Use the ``Guzzle\Batch\NotifyingBatch`` decorator if you want a function to be notified each time the batch queue is
flushed.  This is useful when paired with the flushing batch decorator.  Pass a callable to the ``notify()`` method of
a batch builder to use this decorator with the builder.

.. code-block:: php

    use Guzzle\Batch\BatchBuilder;

    $batch = BatchBuilder::factory()
        ->transferRequests(10)
        ->autoFlushAt(10)
        ->notify(function (array $transferredItems) {
            echo 'Transferred ' . count($transferredItems) . "items\n";
        })
        ->build();

Batch builder method:: ``notify(callable $callback)``

Keeping a history
~~~~~~~~~~~~~~~~~

Use the ``Guzzle\Batch\HistoryBatch`` decorator if you want to maintain a history of all the items transferred with
the batch queue.

.. code-block:: php

    use Guzzle\Batch\BatchBuilder;

    $batch = BatchBuilder::factory()
        ->transferRequests(10)
        ->keepHistory()
        ->build();

After transferring items, you can use the ``getHistory()`` of a batch to retrieve an array of transferred items.  Be
sure to periodically clear the history using ``clearHistory()``.

Batch builder method: ``keepHistory()``

Exception buffering
~~~~~~~~~~~~~~~~~~~

Use the ``Guzzle\Batch\ExceptionBufferingBatch`` decorator to buffer exceptions during a transfer so that you can
transfer as many items as possible then deal with the errored batches after the transfer completes.  After transfer,
use the ``getExceptions()`` method of a batch to retrieve an array of
``Guzzle\Batch\Exception\BatchTransferException`` objects.  You can use these exceptions to attempt to retry the
failed batches.  Be sure to clear the buffered exceptions when you are done with them by using the
``clearExceptions()`` method.

Batch builder method: ``bufferExceptions()``
