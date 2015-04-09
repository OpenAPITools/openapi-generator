==================
Resource iterators
==================

Web services often implement pagination in their responses which requires the end-user to issue a series of consecutive
requests in order to fetch all of the data they asked for. Users of your web service client should not be responsible
for implementing the logic involved in iterating through pages of results. Guzzle provides a simple resource iterator
foundation to make it easier on web service client developers to offer a useful abstraction layer.

Getting an iterator from a client
---------------------------------

    ResourceIteratorInterface Guzzle\Service\Client::getIterator($command [, array $commandOptions, array $iteratorOptions ])

The ``getIterator`` method of a ``Guzzle\Service\ClientInterface`` object provides a convenient interface for
instantiating a resource iterator for a specific command. This method implicitly uses a
``Guzzle\Service\Resource\ResourceIteratorFactoryInterface`` object to create resource iterators. Pass an
instantiated command object or the name of a command in the first argument. When passing the name of a command, the
command factory of the client will create the command by name using the ``$commandOptions`` array. The third argument
may be used to pass an array of options to the constructor of the instantiated ``ResourceIteratorInterface`` object.

.. code-block:: php

    $iterator = $client->getIterator('get_users');

    foreach ($iterator as $user) {
        echo $user['name'] . ' age ' . $user['age'] . PHP_EOL;
    }

The above code sample might execute a single request or a thousand requests. As a consumer of a web service, I don't
care. I just want to iterate over all of the users.

Iterator options
~~~~~~~~~~~~~~~~

The two universal options that iterators should support are ``limit`` and ``page_size``. Using the ``limit`` option
tells the resource iterator to attempt to limit the total number of iterated resources to a specific amount. Keep in
mind that this is not always possible due to limitations that may be inherent to a web service. The ``page_size``
option is used to tell a resource iterator how many resources to request per page of results. Much like the ``limit``
option, you can not rely on getting back exactly the number of resources your specify in the ``page_size`` option.

.. note::

    The ``limit`` and ``page_size`` options can also be specified on an iterator using the ``setLimit($limit)`` and
    ``setPageSize($pageSize)`` methods.

Resolving iterator class names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The default resource iterator factory of a client object expects that your iterators are stored under the ``Model``
folder of your client and that an iterator is names after the CamelCase name of a command followed by the word
"Iterator". For example, if you wanted to create an iterator for the ``get_users`` command, then your iterator class
would be ``Model\GetUsersIterator`` and would be stored in ``Model/GetUsersIterator.php``.

Creating an iterator
--------------------

While not required, resource iterators in Guzzle typically iterate using a ``Guzzle\Service\Command\CommandInterface``
object. ``Guzzle\Service\Resource\ResourceIterator``, the default iterator implementation that you should extend,
accepts a command object and array of iterator options in its constructor. The command object passed to the resource
iterator is expected to be ready to execute and not previously executed. The resource iterator keeps a reference of
this command and clones the original command each time a subsequent request needs to be made to fetch more data.

Implement the sendRequest method
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The most important thing (and usually the only thing) you need to do when creating a resource iterator is to implement
the ``sendRequest()`` method of the resource iterator. The ``sendRequest()`` method is called when you begin
iterating or if there are no resources left to iterate and it you expect to retrieve more resources by making a
subsequent request. The ``$this->command`` property of the resource iterator is updated with a cloned copy of the
original command object passed into the constructor of the iterator. Use this command object to issue your subsequent
requests.

The ``sendRequest()`` method must return an array of the resources you retrieved from making the subsequent call.
Returning an empty array will stop the iteration. If you suspect that your web service client will occasionally return
an empty result set but still requires further iteration, then you must implement a sort of loop in your
``sendRequest()`` method that will continue to issue subsequent requests until your reach the end of the paginated
result set or until additional resources are retrieved from the web service.

Update the nextToken property
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Beyond fetching more results, the ``sendRequest()`` method is responsible for updating the ``$this->nextToken``
property of the iterator. Setting this property to anything other than null tells the iterator that issuing a
subsequent request using the nextToken value will probably return more results. You must continually update this
value in your ``sendRequest()`` method as each response is received from the web service.

Example iterator
----------------

Let's say you want to implement a resource iterator for the ``get_users`` command of your web service. The
``get_users`` command receives a response that contains a list of users, and if there are more pages of results to
retrieve, returns a value called ``next_user``. This return value is known as the **next token** and should be used to
issue subsequent requests.

Assume the response to a ``get_users`` command returns JSON data that looks like this:

.. code-block:: javascript

    {
        "users": [
            { "name": "Craig Johnson", "age": 10 },
            { "name": "Tom Barker", "age": 20 },
            { "name": "Bob Mitchell", "age": 74 }
        ],
        "next_user": "Michael Dowling"
    }

Assume that because there is a ``next_user`` value, there will be more users if a subsequent request is issued. If the
``next_user`` value is missing or null, then we know there are no more results to fetch. Let's implement a resource
iterator for this command.

.. code-block:: php

    namespace MyService\Model;

    use Guzzle\Service\Resource\ResourceIterator;

    /**
     * Iterate over a get_users command
     */
    class GetUsersIterator extends ResourceIterator
    {
        protected function sendRequest()
        {
            // If a next token is set, then add it to the command
            if ($this->nextToken) {
                $this->command->set('next_user', $this->nextToken);
            }

            // Execute the command and parse the result
            $result = $this->command->execute();

            // Parse the next token
            $this->nextToken = isset($result['next_user']) ? $result['next_user'] : false;

            return $result['users'];
        }
    }

As you can see, it's pretty simple to implement an iterator. There are a few things that you should notice from this
example:

1. You do not need to create a new command in the ``sendRequest()`` method. A new command object is cloned from the
   original command passed into the constructor of the iterator before the ``sendRequest()`` method is called.
   Remember that the resource iterator expects a command that has not been executed.
2. When the ``sendRequest()`` method is first called, you will not have a ``$this->nextToken`` value, so always check
   before setting it on a command. Notice that the next token is being updated each time a request is sent.
3. After fetching more resources from the service, always return an array of resources.
