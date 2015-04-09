=============
URI templates
=============

The ``$uri`` passed to one of the client's request creational methods or the base URL of a client can utilize URI
templates. Guzzle supports the entire `URI templates RFC <http://tools.ietf.org/html/rfc6570>`_. URI templates add a
special syntax to URIs that replace template place holders with user defined variables.

Every request created by a Guzzle HTTP client passes through a URI template so that URI template expressions are
automatically expanded:

.. code-block:: php

    $client = new Guzzle\Http\Client('https://example.com/', array('a' => 'hi'));
    $request = $client->get('/{a}');

Because of URI template expansion, the URL of the above request will become ``https://example.com/hi``. Notice that
the template was expanded using configuration variables of the client. You can pass in custom URI template variables
by passing the URI of your request as an array where the first index of the array is the URI template and the second
index of the array are template variables that are merged into the client's configuration variables.

.. code-block:: php

    $request = $client->get(array('/test{?a,b}', array('b' => 'there')));

The URL for this request will become ``https://test.com?a=hi&b=there``. URI templates aren't limited to just simple
variable replacements;  URI templates can provide an enormous amount of flexibility when creating request URIs.

.. code-block:: php

    $request = $client->get(array('http://example.com{+path}{/segments*}{?query,data*}', array(
        'path'     => '/foo/bar',
        'segments' => array('one', 'two'),
        'query'    => 'test',
        'data'     => array(
            'more' => 'value'
        )
    )));

The resulting URL would become ``http://example.com/foo/bar/one/two?query=test&more=value``.

By default, URI template expressions are enclosed in an opening and closing brace (e.g. ``{var}``). If you are working
with a web service that actually uses braces (e.g. Solr), then you can specify a custom regular expression to use to
match URI template expressions.

.. code-block:: php

    $client->getUriTemplate()->setRegex('/\<\$(.+)\>/');
    $client->get('/<$a>');

You can learn about all of the different features of URI templates by reading the
`URI templates RFC <http://tools.ietf.org/html/rfc6570>`_.
