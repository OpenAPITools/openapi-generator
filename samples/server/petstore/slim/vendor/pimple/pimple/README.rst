Pimple
======

.. caution::

    This is the documentation for Pimple 3.x. If you are using Pimple 1.x, read
    the `Pimple 1.x documentation`_. Reading the Pimple 1.x code is also a good
    way to learn more about how to create a simple Dependency Injection
    Container (recent versions of Pimple are more focused on performance).

Pimple is a small Dependency Injection Container for PHP.

Installation
------------

Before using Pimple in your project, add it to your ``composer.json`` file:

.. code-block:: bash

    $ ./composer.phar require pimple/pimple ~3.0

Alternatively, Pimple is also available as a PHP C extension:

.. code-block:: bash

    $ git clone https://github.com/silexphp/Pimple
    $ cd Pimple/ext/pimple
    $ phpize
    $ ./configure
    $ make
    $ make install

Usage
-----

Creating a container is a matter of creating a ``Container`` instance:

.. code-block:: php

    use Pimple\Container;

    $container = new Container();

As many other dependency injection containers, Pimple manages two different
kind of data: **services** and **parameters**.

Defining Services
~~~~~~~~~~~~~~~~~

A service is an object that does something as part of a larger system. Examples
of services: a database connection, a templating engine, or a mailer. Almost
any **global** object can be a service.

Services are defined by **anonymous functions** that return an instance of an
object:

.. code-block:: php

    // define some services
    $container['session_storage'] = function ($c) {
        return new SessionStorage('SESSION_ID');
    };

    $container['session'] = function ($c) {
        return new Session($c['session_storage']);
    };

Notice that the anonymous function has access to the current container
instance, allowing references to other services or parameters.

As objects are only created when you get them, the order of the definitions
does not matter.

Using the defined services is also very easy:

.. code-block:: php

    // get the session object
    $session = $container['session'];

    // the above call is roughly equivalent to the following code:
    // $storage = new SessionStorage('SESSION_ID');
    // $session = new Session($storage);

Defining Factory Services
~~~~~~~~~~~~~~~~~~~~~~~~~

By default, each time you get a service, Pimple returns the **same instance**
of it. If you want a different instance to be returned for all calls, wrap your
anonymous function with the ``factory()`` method

.. code-block:: php

    $container['session'] = $container->factory(function ($c) {
        return new Session($c['session_storage']);
    });

Now, each call to ``$container['session']`` returns a new instance of the
session.

Defining Parameters
~~~~~~~~~~~~~~~~~~~

Defining a parameter allows to ease the configuration of your container from
the outside and to store global values:

.. code-block:: php

    // define some parameters
    $container['cookie_name'] = 'SESSION_ID';
    $container['session_storage_class'] = 'SessionStorage';

If you change the ``session_storage`` service definition like below:

.. code-block:: php

    $container['session_storage'] = function ($c) {
        return new $c['session_storage_class']($c['cookie_name']);
    };

You can now easily change the cookie name by overriding the
``session_storage_class`` parameter instead of redefining the service
definition.

Protecting Parameters
~~~~~~~~~~~~~~~~~~~~~

Because Pimple sees anonymous functions as service definitions, you need to
wrap anonymous functions with the ``protect()`` method to store them as
parameters:

.. code-block:: php

    $container['random_func'] = $container->protect(function () {
        return rand();
    });

Modifying Services after Definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In some cases you may want to modify a service definition after it has been
defined. You can use the ``extend()`` method to define additional code to be
run on your service just after it is created:

.. code-block:: php

    $container['session_storage'] = function ($c) {
        return new $c['session_storage_class']($c['cookie_name']);
    };

    $container->extend('session_storage', function ($storage, $c) {
        $storage->...();

        return $storage;
    });

The first argument is the name of the service to extend, the second a function
that gets access to the object instance and the container.

Extending a Container
~~~~~~~~~~~~~~~~~~~~~

If you use the same libraries over and over, you might want to reuse some
services from one project to the next one; package your services into a
**provider** by implementing ``Pimple\ServiceProviderInterface``:

.. code-block:: php

    use Pimple\Container;

    class FooProvider implements Pimple\ServiceProviderInterface
    {
        public function register(Container $pimple)
        {
            // register some services and parameters
            // on $pimple
        }
    }

Then, register the provider on a Container:

.. code-block:: php

    $pimple->register(new FooProvider());

Fetching the Service Creation Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you access an object, Pimple automatically calls the anonymous function
that you defined, which creates the service object for you. If you want to get
raw access to this function, you can use the ``raw()`` method:

.. code-block:: php

    $container['session'] = function ($c) {
        return new Session($c['session_storage']);
    };

    $sessionFunction = $container->raw('session');

.. _Pimple 1.x documentation: https://github.com/silexphp/Pimple/tree/1.1
