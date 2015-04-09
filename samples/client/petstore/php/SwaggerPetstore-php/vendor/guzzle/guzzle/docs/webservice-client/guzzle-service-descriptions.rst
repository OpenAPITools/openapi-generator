===========================
Guzzle service descriptions
===========================

Guzzle allows you to serialize HTTP requests and parse HTTP responses using a DSL called a service descriptions.
Service descriptions define web service APIs by documenting each operation, the operation's parameters, validation
options for each parameter, an operation's response, how the response is parsed, and any errors that can be raised for
an operation. Writing a service description for a web service allows you to more quickly consume a web service than
writing concrete commands for each web service operation.

Guzzle service descriptions can be representing using a PHP array or JSON document. Guzzle's service descriptions are
heavily inspired by `Swagger <http://swagger.wordnik.com/>`_.

Service description schema
==========================

A Guzzle Service description must match the following JSON schema document. This document can also serve as a guide when
implementing a Guzzle service description.

Download the schema here: :download:`Guzzle JSON schema document </_downloads/guzzle-schema-1.0.json>`

.. class:: overflow-height-500px

    .. literalinclude:: ../_downloads/guzzle-schema-1.0.json
        :language: json

Top-level attributes
--------------------

Service descriptions are comprised of the following top-level attributes:

.. code-block:: json

    {
        "name": "string",
        "apiVersion": "string|number",
        "baseUrl": "string",
        "description": "string",
        "operations": {},
        "models": {},
        "includes": ["string.php", "string.json"]
    }

+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| Property Name                           | Value                   | Description                                                                                                           |
+=========================================+=========================+=======================================================================================================================+
| name                                    | string                  | Name of the web service                                                                                               |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| apiVersion                              | string|number           | Version identifier that the service description is compatible with                                                    |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| baseUrl or basePath                     | string                  | Base URL of the web service. Any relative URI specified in an operation will be merged with the baseUrl using the     |
|                                         |                         | process defined in RFC 2396. Some clients require custom logic to determine the baseUrl. In those cases, it is best   |
|                                         |                         | to not include a baseUrl in the service description, but rather allow the factory method of the client to configure   |
|                                         |                         | the client’s baseUrl.                                                                                                 |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| description                             | string                  | Short summary of the web service                                                                                      |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| operations                              | object containing       | Operations of the service. The key is the name of the operation and value is the attributes of the operation.         |
|                                         | :ref:`operation-schema` |                                                                                                                       |
|                                         |                         |                                                                                                                       |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| models                                  | object containing       | Schema models that can be referenced throughout the service description. Models can be used to define how an HTTP     |
|                                         | :ref:`model-schema`     | response is parsed into a ``Guzzle\Service\Resource\Model`` object when an operation uses a ``model`` ``responseType``|
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| includes                                | array of .js,           | Service description files to include and extend from (can be a .json, .js, or .php file)                              |
|                                         | .json, or .php          |                                                                                                                       |
|                                         | files.                  |                                                                                                                       |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+
| (any additional properties)             | mixed                   | Any additional properties specified as top-level attributes are allowed and will be treated as arbitrary data         |
+-----------------------------------------+-------------------------+-----------------------------------------------------------------------------------------------------------------------+

.. _operation-schema:

Operations
----------

Operations are the actions that can be taken on a service. Each operation is given a unique name and has a distinct
endpoint and HTTP method. If an API has a ``DELETE /users/:id`` operation, a satisfactory operation name might be
``DeleteUser`` with a parameter of ``id`` that is inserted into the URI.

.. class:: overflow-height-250px

    .. code-block:: json

        {
            "operations": {
                "operationName": {
                    "extends": "string",
                    "httpMethod": "GET|POST|PUT|DELETE|PATCH|string",
                    "uri": "string",
                    "summary": "string",
                    "class": "string",
                    "responseClass": "string",
                    "responseNotes": "string",
                    "type": "string",
                    "description": "string",
                    "responseType": "primitive|class|(model by name)|documentation|(string)",
                    "deprecated": false,
                    "errorResponses": [
                        {
                            "code": 500,
                            "reason": "Unexpected Error",
                            "class": "string"
                        }
                    ],
                    "data": {
                        "foo": "bar",
                        "baz": "bam"
                    },
                    "parameters": {}
                }
            }
        }

.. csv-table::
   :header: "Property Name", "Value", "Description"
   :widths: 20, 15, 65

    "extends", "string", "Extend from another operation by name. The parent operation must be defined before the child."
    "httpMethod", "string", "HTTP method used with the operation (e.g. GET, POST, PUT, DELETE, PATCH, etc)"
    "uri", "string", "URI of the operation. The uri attribute can contain URI templates. The variables of the URI template are parameters of the operation with a location value of uri"
    "summary", "string", "Short summary of what the operation does"
    "class", "string", "Custom class to instantiate instead of the default Guzzle\\Service\\Command\\OperationCommand. Using this attribute allows you to define an operation using a service description, but allows more customized logic to be implemented in user-land code."
    "responseClass", "string", "Defined what is returned from the method. Can be a primitive, class name, or model name. You can specify the name of a class to return a more customized result from the operation (for example, a domain model object). When using the name of a PHP class, the class must implement ``Guzzle\Service\Command\ResponseClassInterface``."
    "responseNotes", "string", "A description of the response returned by the operation"
    "responseType", "string", "The type of response that the operation creates: one of primitive, class, model, or documentation. If not specified, this value will be automatically inferred based on whether or not there is a model matching the name, if a matching class name is found, or set to 'primitive' by default."
    "deprecated", "boolean", "Whether or not the operation is deprecated"
    "errorResponses", "array", "Errors that could occur while executing the operation. Each item of the array is an object that can contain a 'code' (HTTP response status code of the error), 'reason' (reason phrase or description of the error), and 'class' (an exception class that will be raised when this error is encountered)"
    "data", "object", "Any arbitrary data to associate with the operation"
    "parameters", "object containing :ref:`parameter-schema` objects", "Parameters of the operation. Parameters are used to define how input data is serialized into a HTTP request."
    "additionalParameters", "A single :ref:`parameter-schema` object", "Validation and serialization rules for any parameter supplied to the operation that was not explicitly defined."

additionalParameters
~~~~~~~~~~~~~~~~~~~~

When a webservice offers a large number of parameters that all are set in the same location (for example the query
string or a JSON document), defining each parameter individually can require a lot of time and repetition. Furthermore,
some web services allow for completely arbitrary parameters to be supplied for an operation. The
``additionalParameters`` attribute can be used to solve both of these issues.

As an example, we can define a Twitter API operation quite easily using ``additionalParameters``. The
GetMentions operation accepts a large number of query string parameters. Defining each of these parameters
is ideal because it provide much more introspection for the client and opens the possibility to use the description with
other tools (e.g. a documentation generator). However, you can very quickly provide a "catch-all" serialization rule
that will place any custom parameters supplied to an operation the generated request's query string parameters.

.. class:: overflow-height-250px

    .. code-block:: json

        {
            "name": "Twitter",
            "apiVersion": "1.1",
            "baseUrl": "https://api.twitter.com/1.1",
            "operations": {
                "GetMentions": {
                    "httpMethod": "GET",
                    "uri": "statuses/mentions_timeline.json",
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

responseClass
~~~~~~~~~~~~~

The ``responseClass`` attribute is used to define the return value of an operation (what is returned by calling the
``getResult()`` method of a command object). The value set in the responseClass attribute can be one of "primitive"
(meaning the result with be primitive type like a string), a class name meaning the result will be an instance of a
specific user-land class, or a model name meaning the result will be a ``Guzzle\Service\Resource\Model`` object that
uses a :ref:`model schema <model-schema>` to define how the HTTP response is parsed.

.. note::

    Using a class name with a ``responseClass`` will only work if it is supported by the ``class`` that is instantiated
    for the operation. Keep this in mind when specifying a custom ``class`` attribute that points to a custom
    ``Guzzle\Service\Command\CommandInterface`` class. The default ``class``,
    ``Guzzle\Service\Command\OperationCommand``, does support setting custom ``class`` attributes.

You can specify the name of a class to return a more customized result from the operation (for example, a domain model
object). When using the name of a PHP class, the class must implement ``Guzzle\Service\Command\ResponseClassInterface``.
Here's a very simple example of implementing a custom responseClass object.

.. code-block:: json

    {
        "operations": {
            "test": {
                "responseClass": "MyApplication\\User"
            }
        }
    }

.. code-block:: php

    namespace MyApplication;

    use Guzzle\Service\Command\ResponseClassInterface;
    use Guzzle\Service\Command\OperationCommand;

    class User implements ResponseClassInterface
    {
        protected $name;

        public static function fromCommand(OperationCommand $command)
        {
            $response = $command->getResponse();
            $xml = $response->xml();

            return new self((string) $xml->name);
        }

        public function __construct($name)
        {
            $this->name = $name;
        }
    }

errorResponses
~~~~~~~~~~~~~~

``errorResponses`` is an array containing objects that define the errors that could occur while executing the
operation. Each item of the array is an object that can contain a 'code' (HTTP response status code of the error),
'reason' (reason phrase or description of the error), and 'class' (an exception class that will be raised when this
error is encountered).

ErrorResponsePlugin
^^^^^^^^^^^^^^^^^^^

Error responses are by default only used for documentation. If you don't need very complex exception logic for your web
service errors, then you can use the ``Guzzle\Plugin\ErrorResponse\ErrorResponsePlugin`` to automatically throw defined
exceptions when one of the ``errorResponse`` rules are matched. The error response plugin will listen for the
``request.complete`` event of a request created by a command object. Every response (including a successful response) is
checked against the list of error responses for an exact match using the following order of checks:

1. Does the errorResponse have a defined ``class``?
2. Is the errorResponse ``code`` equal to the status code of the response?
3. Is the errorResponse ``reason`` equal to the reason phrase of the response?
4. Throw the exception stored in the ``class`` attribute of the errorResponse.

The ``class`` attribute must point to a class that implements
``Guzzle\Plugin\ErrorResponse\ErrorResponseExceptionInterface``. This interface requires that an error response class
implements ``public static function fromCommand(CommandInterface $command, Response $response)``. This method must
return an object that extends from ``\Exception``. After an exception is returned, it is thrown by the plugin.

.. _parameter-schema:

Parameter schema
----------------

Parameters in both operations and models are represented using the
`JSON schema <http://tools.ietf.org/id/draft-zyp-json-schema-04.html>`_ syntax.

.. csv-table::
   :header: "Property Name", "Value", "Description"
   :widths: 20, 15, 65

    "name", "string", "Unique name of the parameter"
    "type", "string|array", "Type of variable (string, number, integer, boolean, object, array, numeric, null, any). Types are using for validation and determining the structure of a parameter. You can use a union type by providing an array of simple types. If one of the union types matches the provided value, then the value is valid."
    "instanceOf", "string", "When the type is an object, you can specify the class that the object must implement"
    "required", "boolean", "Whether or not the parameter is required"
    "default", "mixed", "Default value to use if no value is supplied"
    "static", "boolean", "Set to true to specify that the parameter value cannot be changed from the default setting"
    "description", "string", "Documentation of the parameter"
    "location", "string", "The location of a request used to apply a parameter. Custom locations can be registered with a command, but the defaults are uri, query, statusCode, reasonPhrase, header, body, json, xml, postField, postFile, responseBody"
    "sentAs", "string", "Specifies how the data being modeled is sent over the wire. For example, you may wish to include certain headers in a response model that have a normalized casing of FooBar, but the actual header is x-foo-bar. In this case, sentAs would be set to x-foo-bar."
    "filters", "array", "Array of functions to to run a parameter value through."

filters
~~~~~~~

Each value in the array must be a string containing the full class path to a static method or an array of complex
filter information. You can specify static methods of classes using the full namespace class name followed by
"::" (e.g. ``FooBar::baz()``). Some filters require arguments in order to properly filter a value. For complex filters,
use an object containing a ``method`` attribute pointing to a function, and an ``args`` attribute containing an
array of positional arguments to pass to the function. Arguments can contain keywords that are replaced when filtering
a value: ``@value`` is replaced with the value being filtered, and ``@api`` is replaced with the actual Parameter
object.

.. code-block:: json

    {
        "filters": [
            "strtolower",
            {
                "method": "MyClass::convertString",
                "args": [ "test", "@value", "@api" ]
            }
        ]
    }

The above example will filter a parameter using ``strtolower``. It will then call the ``convertString`` static method
of ``MyClass``, passing in "test", the actual value of the parameter, and a ``Guzzle\Service\Description\Parameter``
object.

Operation parameter location attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The location field of top-level parameters control how a parameter is serialized when generating a request.

uri location
^^^^^^^^^^^^

Parameters are injected into the ``uri`` attribute of the operation using
`URI-template expansion <http://tools.ietf.org/html/rfc6570>`_.

.. code-block:: json

    {
        "operations": {
            "uriTest": {
                "uri": "/test/{testValue}",
                "parameters": {
                    "testValue": {
                        "location": "uri"
                    }
                }
            }
        }
    }

query location
^^^^^^^^^^^^^^

Parameters are injected into the query string of a request. Query values can be nested, which would result in a PHP
style nested query string. The name of a parameter is the default name of the query string parameter added to the
request. You can override this behavior by specifying the ``sentAs`` attribute on the parameter.

.. code-block:: json

    {
        "operations": {
            "queryTest": {
                "parameters": {
                    "testValue": {
                        "location": "query",
                        "sentAs": "test_value"
                    }
                }
            }
        }
    }

header location
^^^^^^^^^^^^^^^

Parameters are injected as headers on an HTTP request. The name of the parameter is used as the name of the header by
default. You can change the name of the header created by the parameter using the ``sentAs`` attribute.

Headers that are of type ``object`` will be added as multiple headers to a request using the key of the input array as
the header key. Setting a ``sentAs`` attribute along with a type ``object`` will use the value of ``sentAs`` as a
prefix for each header key.

body location
^^^^^^^^^^^^^

Parameters are injected as the body of a request. The input of these parameters may be anything that can be cast to a
string or a ``Guzzle\Http\EntityBodyInterface`` object.

postField location
^^^^^^^^^^^^^^^^^^

Parameters are inserted as POST fields in a request. Nested values may be supplied and will be represented using
PHP style nested query strings. The POST field name is the same as the parameter name by default. You can use the
``sentAs`` parameter to override the POST field name.

postFile location
^^^^^^^^^^^^^^^^^

Parameters are added as POST files. A postFile value may be a string pointing to a local filename or a
``Guzzle\Http\Message\PostFileInterface`` object. The name of the POST file will be the name of the parameter by
default. You can use a custom POST file name by using the ``sentAs`` attribute.

Supports "string" and "array" types.

json location
^^^^^^^^^^^^^

Parameters are added to the body of a request as top level keys of a JSON document. Nested values may be specified,
with any number of nested ``Guzzle\Common\ToArrayInterface`` objects. When JSON parameters are specified, the
``Content-Type`` of the request will change to ``application/json`` if a ``Content-Type`` has not already been specified
on the request.

xml location
^^^^^^^^^^^^

Parameters are added to the body of a request as top level nodes of an XML document. Nested values may be specified,
with any number of nested ``Guzzle\Common\ToArrayInterface`` objects. When XML parameters are specified, the
``Content-Type`` of the request will change to ``application/xml`` if a ``Content-Type`` has not already been specified
on the request.

responseBody location
^^^^^^^^^^^^^^^^^^^^^

Specifies the EntityBody of a response. This can be used to download the response body to a file or a custom Guzzle
EntityBody object.

No location
^^^^^^^^^^^

If a parameter has no location attribute, then the parameter is simply used as a data value.

Other locations
^^^^^^^^^^^^^^^

Custom locations can be registered as new locations or override default locations if needed.

.. _model-schema:

Model Schema
------------

Models are used in service descriptions to provide generic JSON schema definitions that can be extended from or used in
``$ref`` attributes. Models can also be referenced in a ``responseClass`` attribute to provide valuable output to an
operation. Models are JSON schema documents and use the exact syntax and attributes used in parameters.

Response Models
~~~~~~~~~~~~~~~

Response models describe how a response is parsed into a ``Guzzle\Service\Resource\Model`` object. Response models are
always modeled as JSON schema objects. When an HTTP response is parsed using a response model, the rules specified on
each property of a response model will translate 1:1 as keys in a PHP associative array. When a ``sentAs`` attribute is
found in response model parameters, the value retrieved from the HTTP response is retrieved using the ``sentAs``
parameter but stored in the response model using the name of the parameter.

The location field of top-level parameters in a response model tell response parsers how data is retrieved from a
response.

statusCode location
^^^^^^^^^^^^^^^^^^^

Retrieves the status code of the response.

reasonPhrase location
^^^^^^^^^^^^^^^^^^^^^

Retrieves the reason phrase of the response.

header location
^^^^^^^^^^^^^^^

Retrieves a header from the HTTP response.

body location
^^^^^^^^^^^^^

Retrieves the body of an HTTP response.

json location
^^^^^^^^^^^^^

Retrieves a top-level parameter from a JSON document contained in an HTTP response.

You can use ``additionalProperties`` if the JSON document is wrapped in an outer array. This allows you to parse the
contents of each item in the array using the parsing rules defined in the ``additionalProperties`` schema.

xml location
^^^^^^^^^^^^

Retrieves a top-level node value from an XML document contained in an HTTP response.

Other locations
^^^^^^^^^^^^^^^

Custom locations can be registered as new locations or override default locations if needed.

Example service description
---------------------------

Let's say you're interacting with a web service called 'Foo' that allows for the following routes and methods::

    GET/POST   /users
    GET/DELETE /users/:id

The following JSON service description implements this simple web service:

.. class:: overflow-height-500px

    .. code-block:: json

        {
            "name": "Foo",
            "apiVersion": "2012-10-14",
            "baseUrl": "http://api.foo.com",
            "description": "Foo is an API that allows you to Baz Bar",
            "operations": {
                "GetUsers": {
                    "httpMethod": "GET",
                    "uri": "/users",
                    "summary": "Gets a list of users",
                    "responseClass": "GetUsersOutput"
                },
                "CreateUser": {
                    "httpMethod": "POST",
                    "uri": "/users",
                    "summary": "Creates a new user",
                    "responseClass": "CreateUserOutput",
                    "parameters": {
                        "name": {
                            "location": "json",
                            "type": "string"
                        },
                        "age": {
                            "location": "json",
                            "type": "integer"
                        }
                    }
                },
                "GetUser": {
                    "httpMethod": "GET",
                    "uri": "/users/{id}",
                    "summary": "Retrieves a single user",
                    "responseClass": "GetUserOutput",
                    "parameters": {
                        "id": {
                            "location": "uri",
                            "description": "User to retrieve by ID",
                            "required": true
                        }
                    }
                },
                "DeleteUser": {
                    "httpMethod": "DELETE",
                    "uri": "/users/{id}",
                    "summary": "Deletes a user",
                    "responseClass": "DeleteUserOutput",
                    "parameters": {
                        "id": {
                            "location": "uri",
                            "description": "User to delete by ID",
                            "required": true
                        }
                    }
                }
            },
            "models": {
                "GetUsersOutput": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {
                                "location": "json",
                                "type": "string"
                            },
                            "age": {
                                "location": "json",
                                "type": "integer"
                            }
                        }
                    }
                },
                "CreateUserOutput": {
                    "type": "object",
                    "properties": {
                        "id": {
                            "location": "json",
                            "type": "string"
                        },
                        "location": {
                            "location": "header",
                            "sentAs": "Location",
                            "type": "string"
                        }
                    }
                },
                "GetUserOutput": {
                    "type": "object",
                    "properties": {
                        "name": {
                            "location": "json",
                            "type": "string"
                        },
                        "age": {
                            "location": "json",
                            "type": "integer"
                        }
                    }
                },
                "DeleteUserOutput": {
                    "type": "object",
                    "properties": {
                        "status": {
                            "location": "statusCode",
                            "type": "integer"
                        }
                    }
                }
            }
        }

If you attach this service description to a client, you would completely configure the client to interact with the
Foo web service and provide valuable response models for each operation.

.. code-block:: php

    use Guzzle\Service\Description\ServiceDescription;

    $description = ServiceDescription::factory('/path/to/client.json');
    $client->setDescription($description);

    $command = $client->getCommand('DeleteUser', array('id' => 123));
    $responseModel = $client->execute($command);
    echo $responseModel['status'];

.. note::

    You can add the service description to your client's factory method or constructor.
