==========================
cURL authentication plugin
==========================

.. warning::

    The CurlAuthPlugin is deprecated. You should use the `auth` parameter of a client to add authorization headers to
    every request created by a client.

    .. code-block:: php

        $client->setDefaultOption('auth', array('username', 'password', 'Basic|Digest|NTLM|Any'));

If your web service client requires basic authorization, then you can use the CurlAuthPlugin to easily add an
Authorization header to each request sent by the client.

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Plugin\CurlAuth\CurlAuthPlugin;

    $client = new Client('http://www.test.com/');

    // Add the auth plugin to the client object
    $authPlugin = new CurlAuthPlugin('username', 'password');
    $client->addSubscriber($authPlugin);

    $response = $client->get('projects/1/people')->send();
    $xml = new SimpleXMLElement($response->getBody(true));
    foreach ($xml->person as $person) {
        echo $person->email . "\n";
    }
