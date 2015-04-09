================
Guzzle iterators
================

Guzzle provides several SPL iterators that can be used with other SPL iterators, including Guzzle resource iterators.
Guzzle's ``guzzle/iterator`` component can also be used independently of the rest of Guzzle through Packagist and
Composer: https://packagist.org/packages/guzzle/iterator

ChunkedIterator
---------------

Pulls out multiple values from an inner iterator and yields and array of values for each outer iteration -- essentially
pulling out chunks of values from the inner iterator.

.. code-block:: php

    use Guzzle\Iterator\ChunkedIterator;

    $inner = new ArrayIterator(range(0, 8));
    $chunkedIterator = new ChunkedIterator($inner, 2);

    foreach ($chunkedIterator as $chunk) {
        echo implode(', ', $chunk) . "\n";
    }

    // >>> 0, 1
    // >>> 2, 3
    // >>> 4, 5
    // >>> 6, 7
    // >>> 8

FilterIterator
--------------

This iterator is used to filter values out of the inner iterator. This iterator can be used when PHP 5.4's
CallbackFilterIterator is not available.

.. code-block:: php

    use Guzzle\Iterator\FilterIterator;

    $inner = new ArrayIterator(range(1, 10));
    $filterIterator = new FilterIterator($inner, function ($value) {
        return $value % 2;
    });

    foreach ($filterIterator as $value) {
        echo $value . "\n";
    }

    // >>> 2
    // >>> 4
    // >>> 6
    // >>> 8
    // >>> 10

MapIterator
-----------

This iterator modifies the values of the inner iterator before yielding.

.. code-block:: php

    use Guzzle\Iterator\MapIterator;

    $inner = new ArrayIterator(range(0, 3));

    $mapIterator = new MapIterator($inner, function ($value) {
        return $value * 10;
    });

    foreach ($mapIterator as $value) {
        echo $value . "\n";
    }

    // >>> 0
    // >>> 10
    // >>> 20
    // >>> 30

MethodProxyIterator
-------------------

This decorator is useful when you need to expose a specific method from an inner iterator that might be wrapper
by one or more iterator decorators. This decorator proxies missing method calls to each inner iterator until one
of the inner iterators can fulfill the call.

.. code-block:: php

    use Guzzle\Iterator\MethodProxyIterator;

    $inner = new \ArrayIterator();
    $proxy = new MethodProxyIterator($inner);

    // Proxy method calls to the ArrayIterator
    $proxy->append('a');
    $proxy->append('b');
