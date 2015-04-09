=================
Welcome to Guzzle
=================

What is Guzzle?
~~~~~~~~~~~~~~~

Guzzle is a PHP HTTP client and framework for building web service clients. Guzzle takes the pain out of sending HTTP
requests and the redundancy out of creating web service clients.

Features at a glance
--------------------

- All the power of cURL with a simple interface.
- Persistent connections and parallel requests.
- Streams request and response bodies
- Service descriptions for quickly building clients.
- Powered by the Symfony2 EventDispatcher.
- Use all of the code or only specific components.
- Plugins for caching, logging, OAuth, mocks, and more
- Includes a custom node.js webserver to test your clients.
- Service descriptions for defining the inputs and outputs of an API
- Resource iterators for traversing paginated resources
- Batching for sending a large number of requests as efficiently as possible

.. code-block:: php

    // Really simple using a static facade
    Guzzle\Http\StaticClient::mount();
    $response = Guzzle::get('http://guzzlephp.org');

    // More control using a client class
    $client = new \Guzzle\Http\Client('http://guzzlephp.org');
    $request = $client->get('/');
    $response = $request->send();

License
-------

Licensed using the `MIT license <http://opensource.org/licenses/MIT>`_.

    Copyright (c) 2013 Michael Dowling <https://github.com/mtdowling>

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.

Contributing
------------

Guidelines
~~~~~~~~~~

This is still a work in progress, but there are only a few rules:

1. Guzzle follows PSR-0, PSR-1, and PSR-2
2. All pull requests must include unit tests to ensure the change works as expected and to prevent future regressions

Reporting a security vulnerability
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We want to ensure that Guzzle is a secure HTTP client library for everyone. If you've discovered a security
vulnerability in Guzzle, we appreciate your help in disclosing it to us in a
`responsible manner <http://en.wikipedia.org/wiki/Responsible_disclosure>`_.

Publicly disclosing a vulnerability can put the entire community at risk. If you've discovered a security concern,
please email us at security@guzzlephp.org. We'll work with you to make sure that we understand the scope of the issue,
and that we fully address your concern. We consider correspondence sent to security@guzzlephp.org our highest priority,
and work to address any issues that arise as quickly as possible.

After a security vulnerability has been corrected, a security hotfix release will be deployed as soon as possible.
