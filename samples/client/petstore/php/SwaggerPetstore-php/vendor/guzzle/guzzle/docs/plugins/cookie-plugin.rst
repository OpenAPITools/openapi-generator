=============
Cookie plugin
=============

Some web services require a Cookie in order to maintain a session. The ``Guzzle\Plugin\Cookie\CookiePlugin`` will add
cookies to requests and parse cookies from responses using a CookieJar object:

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Plugin\Cookie\CookiePlugin;
    use Guzzle\Plugin\Cookie\CookieJar\ArrayCookieJar;

    $cookiePlugin = new CookiePlugin(new ArrayCookieJar());

    // Add the cookie plugin to a client
    $client = new Client('http://www.test.com/');
    $client->addSubscriber($cookiePlugin);

    // Send the request with no cookies and parse the returned cookies
    $client->get('http://www.yahoo.com/')->send();

    // Send the request again, noticing that cookies are being sent
    $request = $client->get('http://www.yahoo.com/');
    $request->send();

    echo $request;

You can disable cookies per-request by setting the ``cookies.disable`` value to true on a request's params object.

.. code-block:: php

    $request->getParams()->set('cookies.disable', true);
