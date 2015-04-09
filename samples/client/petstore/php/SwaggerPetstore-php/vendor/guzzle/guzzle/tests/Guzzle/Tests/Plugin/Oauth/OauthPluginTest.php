<?php

namespace Guzzle\Tests\Plugin\Oauth;

use Guzzle\Http\Message\RequestFactory;
use Guzzle\Http\QueryAggregator\CommaAggregator;
use Guzzle\Plugin\Oauth\OauthPlugin;
use Guzzle\Common\Event;

/**
 * @covers Guzzle\Plugin\Oauth\OauthPlugin
 */
class OauthPluginTest extends \Guzzle\Tests\GuzzleTestCase
{
    const TIMESTAMP = '1327274290';
    const NONCE = 'e7aa11195ca58349bec8b5ebe351d3497eb9e603';

    protected $config = array(
        'consumer_key'    => 'foo',
        'consumer_secret' => 'bar',
        'token'           => 'count',
        'token_secret'    => 'dracula'
    );

    protected function getRequest()
    {
        return RequestFactory::getInstance()->create('POST', 'http://www.test.com/path?a=b&c=d', null, array(
            'e' => 'f'
        ));
    }

    public function testSubscribesToEvents()
    {
        $events = OauthPlugin::getSubscribedEvents();
        $this->assertArrayHasKey('request.before_send', $events);
    }

    public function testAcceptsConfigurationData()
    {
        $p = new OauthPlugin($this->config);

        // Access the config object
        $class = new \ReflectionClass($p);
        $property = $class->getProperty('config');
        $property->setAccessible(true);
        $config = $property->getValue($p);

        $this->assertEquals('foo', $config['consumer_key']);
        $this->assertEquals('bar', $config['consumer_secret']);
        $this->assertEquals('count', $config['token']);
        $this->assertEquals('dracula', $config['token_secret']);
        $this->assertEquals('1.0', $config['version']);
        $this->assertEquals('HMAC-SHA1', $config['signature_method']);
        $this->assertEquals('header', $config['request_method']);
    }

    public function testCreatesStringToSignFromPostRequest()
    {
        $p = new OauthPlugin($this->config);
        $request = $this->getRequest();
        $signString = $p->getStringToSign($request, self::TIMESTAMP, self::NONCE);

        $this->assertContains('&e=f', rawurldecode($signString));

        $expectedSignString =
            // Method and URL
            'POST&http%3A%2F%2Fwww.test.com%2Fpath' .
            // Sorted parameters from query string and body
            '&a%3Db%26c%3Dd%26e%3Df%26oauth_consumer_key%3Dfoo' .
            '%26oauth_nonce%3De7aa11195ca58349bec8b5ebe351d3497eb9e603%26' .
            'oauth_signature_method%3DHMAC-SHA1' .
            '%26oauth_timestamp%3D' . self::TIMESTAMP . '%26oauth_token%3Dcount%26oauth_version%3D1.0';

        $this->assertEquals($expectedSignString, $signString);
    }

    public function testCreatesStringToSignIgnoringPostFields()
    {
        $config = $this->config;
        $config['disable_post_params'] = true;
        $p = new OauthPlugin($config);
        $request = $this->getRequest();
        $sts = rawurldecode($p->getStringToSign($request, self::TIMESTAMP, self::NONCE));
        $this->assertNotContains('&e=f', $sts);
    }

    public function testCreatesStringToSignFromPostRequestWithCustomContentType()
    {
        $p = new OauthPlugin($this->config);
        $request = $this->getRequest();
        $request->setHeader('Content-Type', 'Foo');
        $this->assertEquals(
            // Method and URL
            'POST&http%3A%2F%2Fwww.test.com%2Fpath' .
            // Sorted parameters from query string and body
            '&a%3Db%26c%3Dd%26oauth_consumer_key%3Dfoo' .
            '%26oauth_nonce%3D'. self::NONCE .'%26' .
            'oauth_signature_method%3DHMAC-SHA1' .
            '%26oauth_timestamp%3D' . self::TIMESTAMP . '%26oauth_token%3Dcount%26oauth_version%3D1.0',
            $p->getStringToSign($request, self::TIMESTAMP, self::NONCE)
        );
    }

    /**
     * @depends testCreatesStringToSignFromPostRequest
     */
    public function testConvertsBooleansToStrings()
    {
        $p = new OauthPlugin($this->config);
        $request = $this->getRequest();
        $request->getQuery()->set('a', true);
        $request->getQuery()->set('c', false);
        $this->assertContains('&a%3Dtrue%26c%3Dfalse', $p->getStringToSign($request, self::TIMESTAMP, self::NONCE));
    }

    public function testCreatesStringToSignFromPostRequestWithNullValues()
    {
        $config = array(
            'consumer_key'    => 'foo',
            'consumer_secret' => 'bar',
            'token'           => null,
            'token_secret'    => 'dracula'
        );

        $p          = new OauthPlugin($config);
        $request    = $this->getRequest();
        $signString = $p->getStringToSign($request, self::TIMESTAMP, self::NONCE);

        $this->assertContains('&e=f', rawurldecode($signString));

        $expectedSignString = // Method and URL
                'POST&http%3A%2F%2Fwww.test.com%2Fpath' .
                // Sorted parameters from query string and body
                '&a%3Db%26c%3Dd%26e%3Df%26oauth_consumer_key%3Dfoo' .
                '%26oauth_nonce%3De7aa11195ca58349bec8b5ebe351d3497eb9e603%26' .
                'oauth_signature_method%3DHMAC-SHA1' .
                '%26oauth_timestamp%3D' . self::TIMESTAMP . '%26oauth_version%3D1.0';

        $this->assertEquals($expectedSignString, $signString);
    }

    /**
     * @depends testCreatesStringToSignFromPostRequest
     */
    public function testMultiDimensionalArray()
    {
        $p = new OauthPlugin($this->config);
        $request = $this->getRequest();
        $request->getQuery()->set('a', array('b' => array('e' => 'f', 'c' => 'd')));
        $this->assertContains('a%255Bb%255D%255Bc%255D%3Dd%26a%255Bb%255D%255Be%255D%3Df%26c%3Dd%26e%3Df%26', $p->getStringToSign($request, self::TIMESTAMP, self::NONCE));
    }

    /**
     * @depends testMultiDimensionalArray
     */
    public function testMultiDimensionalArrayWithNonDefaultQueryAggregator()
    {
        $p = new OauthPlugin($this->config);
        $request = $this->getRequest();
        $aggregator = new CommaAggregator();
        $query = $request->getQuery()->setAggregator($aggregator)
            ->set('g', array('h', 'i', 'j'))
            ->set('k', array('l'))
            ->set('m', array('n', 'o'));
        $this->assertContains('a%3Db%26c%3Dd%26e%3Df%26g%3Dh%2Ci%2Cj%26k%3Dl%26m%3Dn%2Co', $p->getStringToSign($request, self::TIMESTAMP, self::NONCE));
    }

    /**
     * @depends testCreatesStringToSignFromPostRequest
     */
    public function testSignsStrings()
    {
        $p = new OauthPlugin(array_merge($this->config, array(
            'signature_callback' => function($string, $key) {
                return "_{$string}|{$key}_";
            }
        )));
        $request = $this->getRequest();
        $sig = $p->getSignature($request, self::TIMESTAMP, self::NONCE);
        $this->assertEquals(
            '_POST&http%3A%2F%2Fwww.test.com%2Fpath&a%3Db%26c%3Dd%26e%3Df%26oauth_consumer_key%3Dfoo' .
            '%26oauth_nonce%3D'. self::NONCE .'%26oauth_signature_method%3DHMAC-SHA1' .
            '%26oauth_timestamp%3D' . self::TIMESTAMP . '%26oauth_token%3Dcount%26oauth_version%3D1.0|' .
            'bar&dracula_',
            base64_decode($sig)
        );
    }

    /**
     * Test that the Oauth is signed correctly and that extra strings haven't been added
     * to the authorization header.
     */
    public function testSignsOauthRequests()
    {
        $p = new OauthPlugin($this->config);
        $event = new Event(array(
            'request' => $this->getRequest(),
            'timestamp' => self::TIMESTAMP
        ));
        $params = $p->onRequestBeforeSend($event);

        $this->assertTrue($event['request']->hasHeader('Authorization'));

        $authorizationHeader = (string)$event['request']->getHeader('Authorization');

        $this->assertStringStartsWith('OAuth ', $authorizationHeader);

        $stringsToCheck = array(
            'oauth_consumer_key="foo"',
            'oauth_nonce="'.urlencode($params['oauth_nonce']).'"',
            'oauth_signature="'.urlencode($params['oauth_signature']).'"',
            'oauth_signature_method="HMAC-SHA1"',
            'oauth_timestamp="' . self::TIMESTAMP . '"',
            'oauth_token="count"',
            'oauth_version="1.0"',
        );

        $totalLength = strlen('OAuth ');

        //Separator is not used before first parameter.
        $separator = '';

        foreach ($stringsToCheck as $stringToCheck) {
            $this->assertContains($stringToCheck, $authorizationHeader);
            $totalLength += strlen($separator);
            $totalLength += strlen($stringToCheck);
            $separator = ', ';
        }

        // Technically this test is not universally valid. It would be allowable to have extra \n characters
        // in the Authorization header. However Guzzle does not do this, so we just perform a simple check
        // on length to validate the Authorization header is composed of only the strings above.
        $this->assertEquals($totalLength, strlen($authorizationHeader), 'Authorization has extra characters i.e. contains extra elements compared to stringsToCheck.');
    }

    public function testSignsOauthQueryStringRequest()
    {
        $config = array_merge(
            $this->config,
            array('request_method' => OauthPlugin::REQUEST_METHOD_QUERY)
        );

        $p = new OauthPlugin($config);
        $event = new Event(array(
            'request' => $this->getRequest(),
            'timestamp' => self::TIMESTAMP
        ));
        $params = $p->onRequestBeforeSend($event);

        $this->assertFalse($event['request']->hasHeader('Authorization'));

        $stringsToCheck = array(
            'a=b',
            'c=d',
            'oauth_consumer_key=foo',
            'oauth_nonce='.urlencode($params['oauth_nonce']),
            'oauth_signature='.urlencode($params['oauth_signature']),
            'oauth_signature_method=HMAC-SHA1',
            'oauth_timestamp='.self::TIMESTAMP,
            'oauth_token=count',
            'oauth_version=1.0',
        );

        $queryString = (string) $event['request']->getQuery();

        $totalLength = strlen('?');

        //Separator is not used before first parameter.
        $separator = '';

        foreach ($stringsToCheck as $stringToCheck) {
            $this->assertContains($stringToCheck, $queryString);
            $totalLength += strlen($separator);
            $totalLength += strlen($stringToCheck);
            $separator = '&';
        }

        // Removes the last query string separator '&'
        $totalLength -= 1;

        $this->assertEquals($totalLength, strlen($queryString), 'Query string has extra characters i.e. contains extra elements compared to stringsToCheck.');
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testInvalidArgumentExceptionOnMethodError()
    {
        $config = array_merge(
            $this->config,
            array('request_method' => 'FakeMethod')
        );

        $p = new OauthPlugin($config);
        $event = new Event(array(
            'request' => $this->getRequest(),
            'timestamp' => self::TIMESTAMP
        ));

        $p->onRequestBeforeSend($event);
    }

    public function testDoesNotAddFalseyValuesToAuthorization()
    {
        unset($this->config['token']);
        $p = new OauthPlugin($this->config);
        $event = new Event(array('request' => $this->getRequest(), 'timestamp' => self::TIMESTAMP));
        $p->onRequestBeforeSend($event);
        $this->assertTrue($event['request']->hasHeader('Authorization'));
        $this->assertNotContains('oauth_token=', (string) $event['request']->getHeader('Authorization'));
    }

    public function testOptionalOauthParametersAreNotAutomaticallyAdded()
    {
        // The only required Oauth parameters are the consumer key and secret. That is enough credentials
        // for signing oauth requests.
         $config = array(
            'consumer_key'    => 'foo',
            'consumer_secret' => 'bar',
        );

        $plugin = new OauthPlugin($config);
        $event = new Event(array(
            'request' => $this->getRequest(),
            'timestamp' => self::TIMESTAMP
        ));

        $timestamp = $plugin->getTimestamp($event);
        $request = $event['request'];
        $nonce = $plugin->generateNonce($request);

        $paramsToSign = $plugin->getParamsToSign($request, $timestamp, $nonce);

        $optionalParams = array(
            'callback'      => 'oauth_callback',
            'token'         => 'oauth_token',
            'verifier'      => 'oauth_verifier',
            'token_secret'  => 'token_secret'
        );

        foreach ($optionalParams as $optionName => $oauthName) {
            $this->assertArrayNotHasKey($oauthName, $paramsToSign, "Optional Oauth param '$oauthName' was not set via config variable '$optionName', but it is listed in getParamsToSign().");
        }
    }
}
