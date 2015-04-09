============
Installation
============

Requirements
------------

#. PHP 5.3.3+ compiled with the cURL extension
#. A recent version of cURL 7.16.2+ compiled with OpenSSL and zlib

Installing Guzzle
-----------------

Composer
~~~~~~~~

The recommended way to install Guzzle is with `Composer <http://getcomposer.org>`_. Composer is a dependency
management tool for PHP that allows you to declare the dependencies your project needs and installs them into your
project.

.. code-block:: bash

    # Install Composer
    curl -sS https://getcomposer.org/installer | php

    # Add Guzzle as a dependency
    php composer.phar require guzzle/guzzle:~3.9

After installing, you need to require Composer's autoloader:

.. code-block:: php

    require 'vendor/autoload.php';

You can find out more on how to install Composer, configure autoloading, and other best-practices for defining
dependencies at `getcomposer.org <http://getcomposer.org>`_.

Using only specific parts of Guzzle
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While you can always just rely on ``guzzle/guzzle``, Guzzle provides several smaller parts of Guzzle as individual
packages available through Composer.

+-----------------------------------------------------------------------------------------------+------------------------------------------+
| Package name                                                                                  | Description                              |
+===============================================================================================+==========================================+
| `guzzle/common <https://packagist.org/packages/guzzle/common>`_                               | Provides ``Guzzle\Common``               |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/http <https://packagist.org/packages/guzzle/http>`_                                   | Provides ``Guzzle\Http``                 |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/parser <https://packagist.org/packages/guzzle/parser>`_                               | Provides ``Guzzle\Parser``               |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/batch <https://packagist.org/packages/guzzle/batch>`_                                 | Provides ``Guzzle\Batch``                |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/cache <https://packagist.org/packages/guzzle/cache>`_                                 | Provides ``Guzzle\Cache``                |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/inflection <https://packagist.org/packages/guzzle/inflection>`_                       | Provides ``Guzzle\Inflection``           |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/iterator <https://packagist.org/packages/guzzle/iterator>`_                           | Provides ``Guzzle\Iterator``             |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/log <https://packagist.org/packages/guzzle/log>`_                                     | Provides ``Guzzle\Log``                  |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin <https://packagist.org/packages/guzzle/plugin>`_                               | Provides ``Guzzle\Plugin`` (all plugins) |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-async <https://packagist.org/packages/guzzle/plugin-async>`_                   | Provides ``Guzzle\Plugin\Async``         |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-backoff <https://packagist.org/packages/guzzle/plugin-backoff>`_               | Provides ``Guzzle\Plugin\BackoffPlugin`` |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-cache <https://packagist.org/packages/guzzle/plugin-cache>`_                   | Provides ``Guzzle\Plugin\Cache``         |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-cookie <https://packagist.org/packages/guzzle/plugin-cookie>`_                 | Provides ``Guzzle\Plugin\Cookie``        |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-error-response <https://packagist.org/packages/guzzle/plugin-error-response>`_ | Provides ``Guzzle\Plugin\ErrorResponse`` |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-history <https://packagist.org/packages/guzzle/plugin-history>`_               | Provides ``Guzzle\Plugin\History``       |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-log <https://packagist.org/packages/guzzle/plugin-log>`_                       | Provides ``Guzzle\Plugin\Log``           |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-md5 <https://packagist.org/packages/guzzle/plugin-md5>`_                       | Provides ``Guzzle\Plugin\Md5``           |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-mock <https://packagist.org/packages/guzzle/plugin-mock>`_                     | Provides ``Guzzle\Plugin\Mock``          |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/plugin-oauth <https://packagist.org/packages/guzzle/plugin-oauth>`_                   | Provides ``Guzzle\Plugin\Oauth``         |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/service <https://packagist.org/packages/guzzle/service>`_                             | Provides ``Guzzle\Service``              |
+-----------------------------------------------------------------------------------------------+------------------------------------------+
| `guzzle/stream <https://packagist.org/packages/guzzle/stream>`_                               | Provides ``Guzzle\Stream``               |
+-----------------------------------------------------------------------------------------------+------------------------------------------+

Bleeding edge
^^^^^^^^^^^^^

During your development, you can keep up with the latest changes on the master branch by setting the version
requirement for Guzzle to ``dev-master``.

.. code-block:: js

   {
      "require": {
         "guzzle/guzzle": "dev-master"
      }
   }

PEAR
~~~~

Guzzle can be installed through PEAR:

.. code-block:: bash

    pear channel-discover guzzlephp.org/pear
    pear install guzzle/guzzle

You can install a specific version of Guzzle by providing a version number suffix:

.. code-block:: bash

    pear install guzzle/guzzle-3.9.0

Contributing to Guzzle
----------------------

In order to contribute, you'll need to checkout the source from GitHub and install Guzzle's dependencies using
Composer:

.. code-block:: bash

    git clone https://github.com/guzzle/guzzle.git
    cd guzzle && curl -s http://getcomposer.org/installer | php && ./composer.phar install --dev

Guzzle is unit tested with PHPUnit. You will need to create your own phpunit.xml file in order to run the unit tests
(or just copy phpunit.xml.dist to phpunit.xml). Run the tests using the vendored PHPUnit binary:

.. code-block:: bash

    vendor/bin/phpunit

You'll need to install node.js v0.5.0 or newer in order to test the cURL implementation.

Framework integrations
----------------------

Using Guzzle with Symfony
~~~~~~~~~~~~~~~~~~~~~~~~~

Bundles are available on GitHub:

- `DdeboerGuzzleBundle <https://github.com/ddeboer/GuzzleBundle>`_ for Guzzle 2
- `MisdGuzzleBundle <https://github.com/misd-service-development/guzzle-bundle>`_ for Guzzle 3

Using Guzzle with Silex
~~~~~~~~~~~~~~~~~~~~~~~

A `Guzzle Silex service provider <https://github.com/guzzle/guzzle-silex-extension>`_ is available on GitHub.
