======================
The web service client
======================

The ``Guzzle\Service`` namespace contains various abstractions that help to make it easier to interact with a web
service API, including commands, service descriptions, and resource iterators.

In this chapter, we'll build a simple `Twitter API client <https://dev.twitter.com/docs/api/1.1>`_.

Creating a client
=================

A class that extends from ``Guzzle\Service\Client`` or implements ``Guzzle\Service\ClientInterface`` must implement a
``factory()`` method in order to be used with a :doc:`service builder <using-the-service-builder>`.

Factory method
--------------

You can use the ``factory()`` method of a client directly if you do not need a service builder.

.. code-block:: php

    use mtdowling\TwitterClient;

    // Create a client and pass an array of configuration data
    $twitter = TwitterClient::factory(array(
        'consumer_key'    => '****',
        'consumer_secret' => '****',
        'token'           => '****',
        'token_secret'    => '****'
    ));

.. note::

    If you'd like to follow along, here's how to get your Twitter API credentials:

    1. Visit https://dev.twitter.com/apps
    2. Click on an application that you've created
    3. Click on the "OAuth tool" tab
    4. Copy all of the settings under "OAuth Settings"

Implementing a factory method
-----------------------------

Creating a client and its factory method is pretty simple. You just need to implement ``Guzzle\Service\ClientInterface``
or extend from ``Guzzle\Service\Client``.

.. code-block:: php

    namespace mtdowling;

    use Guzzle\Common\Collection;
    use Guzzle\Plugin\Oauth\OauthPlugin;
    use Guzzle\Service\Client;
    use Guzzle\Service\Description\ServiceDescription;

    /**
     * A simple Twitter API client
     */
    class TwitterClient extends Client
    {
        public static function factory($config = array())
        {
            // Provide a hash of default client configuration options
            $default = array('base_url' => 'https://api.twitter.com/1.1');

            // The following values are required when creating the client
            $required = array(
                'base_url',
                'consumer_key',
                'consumer_secret',
                'token',
                'token_secret'
            );

            // Merge in default settings and validate the config
            $config = Collection::fromConfig($config, $default, $required);

            // Create a new Twitter client
            $client = new self($config->get('base_url'), $config);

            // Ensure that the OauthPlugin is attached to the client
            $client->addSubscriber(new OauthPlugin($config->toArray()));

            return $client;
        }
    }

Service Builder
---------------

A service builder is used to easily create web service clients, provides a simple configuration driven approach to
creating clients, and allows you to share configuration settings across multiple clients. You can find out more about
Guzzle's service builder in :doc:`using-the-service-builder`.

.. code-block:: php

    use Guzzle\Service\Builder\ServiceBuilder;

    // Create a service builder and provide client configuration data
    $builder = ServiceBuilder::factory('/path/to/client_config.json');

    // Get the client from the service builder by name
    $twitter = $builder->get('twitter');

The above example assumes you have JSON data similar to the following stored in "/path/to/client_config.json":

.. code-block:: json

    {
        "services": {
            "twitter": {
                "class": "mtdowling\\TwitterClient",
                "params": {
                    "consumer_key": "****",
                    "consumer_secret": "****",
                    "token": "****",
                    "token_secret": "****"
                }
            }
        }
    }

.. note::

    A service builder becomes much more valuable when using multiple web service clients in a single application or
    if you need to utilize the same client with varying configuration settings (e.g. multiple accounts).

Commands
========

Commands are a concept in Guzzle that helps to hide the underlying implementation of an API by providing an easy to use
parameter driven object for each action of an API. A command is responsible for accepting an array of configuration
parameters, serializing an HTTP request, and parsing an HTTP response. Following the
`command pattern <http://en.wikipedia.org/wiki/Command_pattern>`_, commands in Guzzle offer a greater level of
flexibility when implementing and utilizing a web service client.

Executing commands
------------------

You must explicitly execute a command after creating a command using the ``getCommand()`` method. A command has an
``execute()`` method that may be called, or you can use the ``execute()`` method of a client object and pass in the
command object. Calling either of these execute methods will return the result value of the command. The result value is
the result of parsing the HTTP response with the ``process()`` method.

.. code-block:: php

    // Get a command from the client and pass an array of parameters
    $command = $twitter->getCommand('getMentions', array(
        'count' => 5
    ));

    // Other parameters can be set on the command after it is created
    $command['trim_user'] = false;

    // Execute the command using the command object.
    // The result value contains an array of JSON data from the response
    $result = $command->execute();

    // You can retrieve the result of the command later too
    $result = $command->getResult().

Command object also contains methods that allow you to inspect the HTTP request and response that was utilized with
the command.

.. code-block:: php

    $request = $command->getRequest();
    $response = $command->getResponse();

.. note::

    The format and notation used to retrieve commands from a client can be customized by injecting a custom command
    factory, ``Guzzle\Service\Command\Factory\FactoryInterface``, on the client using ``$client->setCommandFactory()``.

Executing with magic methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using method missing magic methods with a command, the command will be executed right away and the result of the
command is returned.

.. code-block:: php

    $jsonData = $twitter->getMentions(array(
        'count'     => 5,
        'trim_user' => true
    ));

Creating commands
-----------------

Commands are created using either the ``getCommand()`` method of a client or a magic missing method of a client. Using
the ``getCommand()`` method allows you to create a command without executing it, allowing for customization of the
command or the request serialized by the command.

When a client attempts to create a command, it uses the client's ``Guzzle\Service\Command\Factory\FactoryInterface``.
By default, Guzzle will utilize a command factory that first looks for a concrete class for a particular command
(concrete commands) followed by a command defined by a service description (operation commands). We'll learn more about
concrete commands and operation commands later in this chapter.

.. code-block:: php

    // Get a command from the twitter client.
    $command = $twitter->getCommand('getMentions');
    $result = $command->execute();

Unless you've skipped ahead, running the above code will throw an exception.

    PHP Fatal error:  Uncaught exception 'Guzzle\Common\Exception\InvalidArgumentException' with message
    'Command was not found matching getMentions'

This exception was thrown because the "getMentions" command has not yet been implemented. Let's implement one now.

Concrete commands
~~~~~~~~~~~~~~~~~

Commands can be created in one of two ways: create a concrete command class that extends
``Guzzle\Service\Command\AbstractCommand`` or
:doc:`create an OperationCommand based on a service description <guzzle-service-descriptions>`. The recommended
approach is to use a service description to define your web service, but you can use concrete commands when custom
logic must be implemented for marshaling or unmarshaling a HTTP message.

Commands are the method in which you abstract away the underlying format of the requests that need to be sent to take
action on a web service. Commands in Guzzle are meant to be built by executing a series of setter methods on a command
object. Commands are only validated right before they are executed. A ``Guzzle\Service\Client`` object is responsible
for executing commands. Commands created for your web service must implement
``Guzzle\Service\Command\CommandInterface``, but it's easier to extend the ``Guzzle\Service\Command\AbstractCommand``
class, implement the ``build()`` method, and optionally implement the ``process()`` method.

Serializing requests
^^^^^^^^^^^^^^^^^^^^

The ``build()`` method of a command is responsible for using the arguments of the command to build and serialize a
HTTP request and set the request on the ``$request`` property of the command object. This step is usually taken care of
for you when using a service description driven command that uses the default
``Guzzle\Service\Command\OperationCommand``. You may wish to implement the process method yourself when you aren't
using a service description or need to implement more complex request serialization.

.. important::::

    When implementing a custom ``build()`` method, be sure to set the class property of ``$this->request`` to an
    instantiated and ready to send request.

The following example shows how to implement the ``getMentions``
`Twitter API <https://dev.twitter.com/docs/api/1.1/get/statuses/mentions_timeline>`_ method using a concrete command.

.. code-block:: php

    namespace mtdowling\Twitter\Command;

    use Guzzle\Service\Command\AbstractCommand;

    class GetMentions extends AbstractCommand
    {
        protected function build()
        {
            // Create the request property of the command
            $this->request = $this->client->get('statuses/mentions_timeline.json');

            // Grab the query object of the request because we will use it for
            // serializing command parameters on the request
            $query = $this->request->getQuery();

            if ($this['count']) {
                $query->set('count', $this['count']);
            }

            if ($this['since_id']) {
                $query->set('since_id', $this['since_id']);
            }

            if ($this['max_id']) {
                $query->set('max_id', $this['max_id']);
            }

            if ($this['trim_user'] !== null) {
                $query->set('trim_user', $this['trim_user'] ? 'true' : 'false');
            }

            if ($this['contributor_details'] !== null) {
                $query->set('contributor_details', $this['contributor_details'] ? 'true' : 'false');
            }

            if ($this['include_entities'] !== null) {
                $query->set('include_entities', $this['include_entities'] ? 'true' : 'false');
            }
        }
    }

By default, a client will attempt to find concrete command classes under the ``Command`` namespace of a client. First
the client will attempt to find an exact match for the name of the command to the name of the command class. If an
exact match is not found, the client will calculate a class name using inflection. This is calculated based on the
folder hierarchy of a command and converting the CamelCased named commands into snake_case. Here are some examples on
how the command names are calculated:

#. ``Foo\Command\JarJar`` **->** jar_jar
#. ``Foo\Command\Test`` **->** test
#. ``Foo\Command\People\GetCurrentPerson`` **->** people.get_current_person

Notice how any sub-namespace beneath ``Command`` is converted from ``\`` to ``.`` (a period). CamelCasing is converted
to lowercased snake_casing (e.g. JarJar == jar_jar).

Parsing responses
^^^^^^^^^^^^^^^^^

The ``process()`` method of a command is responsible for converting an HTTP response into something more useful. For
example, a service description operation that has specified a model object in the ``responseClass`` attribute of the
operation will set a ``Guzzle\Service\Resource\Model`` object as the result of the command. This behavior can be
completely modified as needed-- even if you are using operations and responseClass models. Simply implement a custom
``process()`` method that sets the ``$this->result`` class property to whatever you choose. You can reuse parts of the
default Guzzle response parsing functionality or get inspiration from existing code by using
``Guzzle\Service\Command\OperationResponseParser`` and ``Guzzle\Service\Command\DefaultResponseParser`` classes.

If you do not implement a custom ``process()`` method and are not using a service description, then Guzzle will attempt
to guess how a response should be processed based on the Content-Type header of the response. Because the Twitter API
sets a ``Content-Type: application/json`` header on this response, we do not need to implement any custom response
parsing.

Operation commands
~~~~~~~~~~~~~~~~~~

Operation commands are commands in which the serialization of an HTTP request and the parsing of an HTTP response are
driven by a Guzzle service description. Because request serialization, validation, and response parsing are
described using a DSL, creating operation commands is a much faster process than writing concrete commands.

Creating operation commands for our Twitter client can remove a great deal of redundancy from the previous concrete
command, and allows for a deeper runtime introspection of the API. Here's an example service description we can use to
create the Twitter API client:

.. code-block:: json

    {
        "name": "Twitter",
        "apiVersion": "1.1",
        "baseUrl": "https://api.twitter.com/1.1",
        "description": "Twitter REST API client",
        "operations": {
            "GetMentions": {
                "httpMethod": "GET",
                "uri": "statuses/mentions_timeline.json",
                "summary": "Returns the 20 most recent mentions for the authenticating user.",
                "responseClass": "GetMentionsOutput",
                "parameters": {
                    "count": {
                        "description": "Specifies the number of tweets to try and retrieve",
                        "type": "integer",
                        "location": "query"
                    },
                    "since_id": {
                        "description": "Returns results with an ID greater than the specified ID",
                        "type": "integer",
                        "location": "query"
                    },
                    "max_id": {
                        "description": "Returns results with an ID less than or equal to the specified ID.",
                        "type": "integer",
                        "location": "query"
                    },
                    "trim_user": {
                        "description": "Limits the amount of data returned for each user",
                        "type": "boolean",
                        "location": "query"
                    },
                    "contributor_details": {
                        "description": "Adds more data to contributor elements",
                        "type": "boolean",
                        "location": "query"
                    },
                    "include_entities": {
                        "description": "The entities node will be disincluded when set to false.",
                        "type": "boolean",
                        "location": "query"
                    }
                }
            }
        },
        "models": {
            "GetMentionsOutput": {
                "type": "object",
                "additionalProperties": {
                    "location": "json"
                }
            }
        }
    }

If you're lazy, you can define the API in a less descriptive manner using ``additionalParameters``.
``additionalParameters`` define the serialization and validation rules of parameters that are not explicitly defined
in a service description.

.. code-block:: json

    {
        "name": "Twitter",
        "apiVersion": "1.1",
        "baseUrl": "https://api.twitter.com/1.1",
        "description": "Twitter REST API client",
        "operations": {
            "GetMentions": {
                "httpMethod": "GET",
                "uri": "statuses/mentions_timeline.json",
                "summary": "Returns the 20 most recent mentions for the authenticating user.",
                "responseClass": "GetMentionsOutput",
                "additionalParameters": {
                    "location": "query"
                }
            }
        },
        "models": {
            "GetMentionsOutput": {
                "type": "object",
                "additionalProperties": {
                    "location": "json"
                }
            }
        }
    }

You should attach the service description to the client at the end of the client's factory method:

.. code-block:: php

    // ...
    class TwitterClient extends Client
    {
        public static function factory($config = array())
        {
            // ... same code as before ...

            // Set the service description
            $client->setDescription(ServiceDescription::factory('path/to/twitter.json'));

            return $client;
        }
    }

The client can now use operations defined in the service description instead of requiring you to create concrete
command classes. Feel free to delete the concrete command class we created earlier.

.. code-block:: php

    $jsonData = $twitter->getMentions(array(
        'count'     => 5,
        'trim_user' => true
    ));

Executing commands in parallel
------------------------------

Much like HTTP requests, Guzzle allows you to send multiple commands in parallel. You can send commands in parallel by
passing an array of command objects to a client's ``execute()`` method. The client will serialize each request and
send them all in parallel. If an error is encountered during the transfer, then a
``Guzzle\Service\Exception\CommandTransferException`` is thrown, which allows you to retrieve a list of commands that
succeeded and a list of commands that failed.

.. code-block:: php

    use Guzzle\Service\Exception\CommandTransferException;

    $commands = array();
    $commands[] = $twitter->getCommand('getMentions');
    $commands[] = $twitter->getCommand('otherCommandName');
    // etc...

    try {
        $result = $client->execute($commands);
        foreach ($result as $command) {
            echo $command->getName() . ': ' . $command->getResponse()->getStatusCode() . "\n";
        }
    } catch (CommandTransferException $e) {
        // Get an array of the commands that succeeded
        foreach ($e->getSuccessfulCommands() as $command) {
            echo $command->getName() . " succeeded\n";
        }
        // Get an array of the commands that failed
        foreach ($e->getFailedCommands() as $command) {
            echo $command->getName() . " failed\n";
        }
    }

.. note::

    All commands executed from a client using an array must originate from the same client.

Special command options
-----------------------

Guzzle exposes several options that help to control how commands are validated, serialized, and parsed.
Command options can be specified when creating a command or in the ``command.params`` parameter in the
``Guzzle\Service\Client``.

=========================== ============================================================================================
command.request_options     Option used to add :ref:`Request options <request-options>` to the request created by a
                            command
command.hidden_params       An array of the names of parameters ignored by the ``additionalParameters`` parameter schema
command.disable_validation  Set to true to disable JSON schema validation of the command's input parameters
command.response_processing Determines how the default response parser will parse the command. One of "raw" no parsing,
                            "model" (the default method used to parse commands using response models defined in service
                            descriptions)
command.headers             (deprecated) Option used to specify custom headers.  Use ``command.request_options`` instead
command.on_complete         (deprecated) Option used to add an onComplete method to a command.  Use
                            ``command.after_send`` event instead
command.response_body       (deprecated) Option used to change the entity body used to store a response.
                            Use ``command.request_options`` instead
=========================== ============================================================================================

Advanced client configuration
=============================

Default command parameters
--------------------------

When creating a client object, you can specify default command parameters to pass into all commands. Any key value pair
present in the ``command.params`` settings of a client will be added as default parameters to any command created
by the client.

.. code-block:: php

    $client = new Guzzle\Service\Client(array(
        'command.params' => array(
            'default_1' => 'foo',
            'another'   => 'bar'
        )
    ));

Magic methods
-------------

Client objects will, by default, attempt to create and execute commands when a missing method is invoked on a client.
This powerful concept applies to both concrete commands and operation commands powered by a service description. This
makes it appear to the end user that you have defined actual methods on a client object, when in fact, the methods are
invoked using PHP's magic ``__call`` method.

The ``__call`` method uses the ``getCommand()`` method of a client, which uses the client's internal
``Guzzle\Service\Command\Factory\FactoryInterface`` object. The default command factory allows you to instantiate
operations defined in a client's service description. The method in which a client determines which command to
execute is defined as follows:

1. The client will first try to find a literal match for an operation in the service description.
2. If the literal match is not found, the client will try to uppercase the first character of the operation and find
   the match again.
3. If a match is still not found, the command factory will inflect the method name from CamelCase to snake_case and
   attempt to find a matching command.
4. If a command still does not match, an exception is thrown.

.. code-block:: php

    // Use the magic method
    $result = $twitter->getMentions();

    // This is exactly the same as:
    $result = $twitter->getCommand('getMentions')->execute();

You can disable magic methods on a client by passing ``false`` to the ``enableMagicMethod()`` method.

Custom command factory
----------------------

A client by default uses the ``Guzzle\Service\Command\Factory\CompositeFactory`` which allows multiple command
factories to attempt to create a command by a certain name. The default CompositeFactory uses a ``ConcreteClassFactory``
and a ``ServiceDescriptionFactory`` if a service description is specified on a client. You can specify a custom
command factory if your client requires custom command creation logic using the ``setCommandFactory()`` method of
a client.

Custom resource Iterator factory
--------------------------------

Resource iterators can be retrieved from a client using the ``getIterator($name)`` method of a client. This method uses
a client's internal ``Guzzle\Service\Resource\ResourceIteratorFactoryInterface`` object. A client by default uses a
``Guzzle\Service\Resource\ResourceIteratorClassFactory`` to attempt to find concrete classes that implement resource
iterators. The default factory will first look for matching iterators in the ``Iterator`` subdirectory of the client
followed by the ``Model`` subdirectory of a client. Use the ``setResourceIteratorFactory()`` method of a client to
specify a custom resource iterator factory.

Plugins and events
==================

``Guzzle\Service\Client`` exposes various events that allow you to hook in custom logic. A client object owns a
``Symfony\Component\EventDispatcher\EventDispatcher`` object that can be accessed by calling
``$client->getEventDispatcher()``. You can use the event dispatcher to add listeners (a simple callback function) or
event subscribers (classes that listen to specific events of a dispatcher).

.. _service-client-events:

Events emitted from a Service Client
------------------------------------

A ``Guzzle\Service\Client`` object emits the following events:

+------------------------------+--------------------------------------------+------------------------------------------+
| Event name                   | Description                                | Event data                               |
+==============================+============================================+==========================================+
| client.command.create        | The client created a command object        | * client: Client object                  |
|                              |                                            | * command: Command object                |
+------------------------------+--------------------------------------------+------------------------------------------+
| command.before_prepare       | Before a command is validated and built.   | * command: Command being prepared        |
|                              | This is also before a request is created.  |                                          |
+------------------------------+--------------------------------------------+------------------------------------------+
| command.after_prepare        | After a command instantiates and           | * command: Command that was prepared     |
|                              | configures its request object.             |                                          |
+------------------------------+--------------------------------------------+------------------------------------------+
| command.before_send          | The client is about to execute a prepared  | * command: Command to execute            |
|                              | command                                    |                                          |
+------------------------------+--------------------------------------------+------------------------------------------+
| command.after_send           | The client successfully completed          | * command: The command that was executed |
|                              | executing a command                        |                                          |
+------------------------------+--------------------------------------------+------------------------------------------+
| command.parse_response       | Called when ``responseType`` is ``class``  | * command: The command with a response   |
|                              | and the response is about to be parsed.    |   about to be parsed.                    |
+------------------------------+--------------------------------------------+------------------------------------------+

.. code-block:: php

    use Guzzle\Common\Event;
    use Guzzle\Service\Client;

    $client = new Client();

    // create an event listener that operates on request objects
    $client->getEventDispatcher()->addListener('command.after_prepare', function (Event $event) {
        $command = $event['command'];
        $request = $command->getRequest();

        // do something with request
    });

.. code-block:: php

    use Guzzle\Common\Event;
    use Guzzle\Common\Client;
    use Symfony\Component\EventDispatcher\EventSubscriberInterface;

    class EventSubscriber implements EventSubscriberInterface
    {
        public static function getSubscribedEvents()
        {
            return array(
                'client.command.create' => 'onCommandCreate',
                'command.parse_response' => 'onParseResponse'
            );
        }

        public function onCommandCreate(Event $event)
        {
            $client = $event['client'];
            $command = $event['command'];
            // operate on client and command
        }

        public function onParseResponse(Event $event)
        {
            $command = $event['command'];
            // operate on the command
        }
    }

    $client = new Client();

    $client->addSubscriber(new EventSubscriber());
