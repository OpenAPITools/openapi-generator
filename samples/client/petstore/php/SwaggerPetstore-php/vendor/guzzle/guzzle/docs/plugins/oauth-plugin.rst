============
OAuth plugin
============

Guzzle ships with an OAuth 1.0 plugin that can sign requests using a consumer key, consumer secret, OAuth token,
and OAuth secret. Here's an example showing how to send an authenticated request to the Twitter REST API:

.. code-block:: php

    use Guzzle\Http\Client;
    use Guzzle\Plugin\Oauth\OauthPlugin;

    $client = new Client('http://api.twitter.com/1');
    $oauth = new OauthPlugin(array(
        'consumer_key'    => 'my_key',
        'consumer_secret' => 'my_secret',
        'token'           => 'my_token',
        'token_secret'    => 'my_token_secret'
    ));
    $client->addSubscriber($oauth);

    $response = $client->get('statuses/public_timeline.json')->send();

If you need to use a custom signing method, you can pass a ``signature_method`` configuration option in the
constructor of the OAuth plugin. The ``signature_method`` option must be a callable variable that accepts a string to
sign and signing key and returns a signed string.

.. note::

    You can omit the ``token`` and ``token_secret`` options to use two-legged OAuth.
