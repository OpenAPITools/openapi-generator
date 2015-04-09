<?php

namespace Guzzle\Tests\Http\Message;

use Guzzle\Common\Collection;
use Guzzle\Http\Client;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Url;
use Guzzle\Http\EntityBody;
use Guzzle\Http\Message\RequestFactory;
use Guzzle\Http\Message\Request;
use Guzzle\Http\QueryString;
use Guzzle\Parser\Message\MessageParser;
use Guzzle\Plugin\Log\LogPlugin;
use Guzzle\Plugin\Mock\MockPlugin;

/**
 * @group server
 * @covers Guzzle\Http\Message\RequestFactory
 */
class HttpRequestFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testCachesSingletonInstance()
    {
        $factory = RequestFactory::getInstance();
        $this->assertSame($factory, RequestFactory::getInstance());
    }

    public function testCreatesNewGetRequests()
    {
        $request = RequestFactory::getInstance()->create('GET', 'http://www.google.com/');
        $this->assertInstanceOf('Guzzle\\Http\\Message\\MessageInterface', $request);
        $this->assertInstanceOf('Guzzle\\Http\\Message\\RequestInterface', $request);
        $this->assertInstanceOf('Guzzle\\Http\\Message\\Request', $request);
        $this->assertEquals('GET', $request->getMethod());
        $this->assertEquals('http', $request->getScheme());
        $this->assertEquals('http://www.google.com/', $request->getUrl());
        $this->assertEquals('www.google.com', $request->getHost());
        $this->assertEquals('/', $request->getPath());
        $this->assertEquals('/', $request->getResource());

        // Create a GET request with a custom receiving body
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $b = EntityBody::factory();
        $request = RequestFactory::getInstance()->create('GET', $this->getServer()->getUrl(), null, $b);
        $request->setClient(new Client());
        $response = $request->send();
        $this->assertSame($b, $response->getBody());
    }

    public function testCreatesPutRequests()
    {
        // Test using a string
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.google.com/path?q=1&v=2', null, 'Data');
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('PUT', $request->getMethod());
        $this->assertEquals('http', $request->getScheme());
        $this->assertEquals('http://www.google.com/path?q=1&v=2', $request->getUrl());
        $this->assertEquals('www.google.com', $request->getHost());
        $this->assertEquals('/path', $request->getPath());
        $this->assertEquals('/path?q=1&v=2', $request->getResource());
        $this->assertInstanceOf('Guzzle\\Http\\EntityBody', $request->getBody());
        $this->assertEquals('Data', (string) $request->getBody());
        unset($request);

        // Test using an EntityBody
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.google.com/path?q=1&v=2', null, EntityBody::factory('Data'));
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('Data', (string) $request->getBody());

        // Test using a resource
        $resource = fopen('php://temp', 'w+');
        fwrite($resource, 'Data');
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.google.com/path?q=1&v=2', null, $resource);
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('Data', (string) $request->getBody());

        // Test using an object that can be cast as a string
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.google.com/path?q=1&v=2', null, Url::factory('http://www.example.com/'));
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('http://www.example.com/', (string) $request->getBody());
    }

    public function testCreatesHeadAndDeleteRequests()
    {
        $request = RequestFactory::getInstance()->create('DELETE', 'http://www.test.com/');
        $this->assertEquals('DELETE', $request->getMethod());
        $request = RequestFactory::getInstance()->create('HEAD', 'http://www.test.com/');
        $this->assertEquals('HEAD', $request->getMethod());
    }

    public function testCreatesOptionsRequests()
    {
        $request = RequestFactory::getInstance()->create('OPTIONS', 'http://www.example.com/');
        $this->assertEquals('OPTIONS', $request->getMethod());
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
    }

    public function testCreatesNewPutRequestWithBody()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.google.com/path?q=1&v=2', null, 'Data');
        $this->assertEquals('Data', (string) $request->getBody());
    }

    public function testCreatesNewPostRequestWithFields()
    {
        // Use an array
        $request = RequestFactory::getInstance()->create('POST', 'http://www.google.com/path?q=1&v=2', null, array(
            'a' => 'b'
        ));
        $this->assertEquals(array('a' => 'b'), $request->getPostFields()->getAll());
        unset($request);

        // Use a collection
        $request = RequestFactory::getInstance()->create('POST', 'http://www.google.com/path?q=1&v=2', null, new Collection(array(
            'a' => 'b'
        )));
        $this->assertEquals(array('a' => 'b'), $request->getPostFields()->getAll());

        // Use a QueryString
        $request = RequestFactory::getInstance()->create('POST', 'http://www.google.com/path?q=1&v=2', null, new QueryString(array(
            'a' => 'b'
        )));
        $this->assertEquals(array('a' => 'b'), $request->getPostFields()->getAll());

        $request = RequestFactory::getInstance()->create('POST', 'http://www.test.com/', null, array(
            'a' => 'b',
            'file' => '@' . __FILE__
        ));

        $this->assertEquals(array(
            'a' => 'b'
        ), $request->getPostFields()->getAll());

        $files = $request->getPostFiles();
        $this->assertInstanceOf('Guzzle\Http\Message\PostFile', $files['file'][0]);
    }

    public function testCreatesFromParts()
    {
        $parts = parse_url('http://michael:123@www.google.com:8080/path?q=1&v=2');

        $request = RequestFactory::getInstance()->fromParts('PUT', $parts, null, 'Data');
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('PUT', $request->getMethod());
        $this->assertEquals('http', $request->getScheme());
        $this->assertEquals('http://www.google.com:8080/path?q=1&v=2', $request->getUrl());
        $this->assertEquals('www.google.com', $request->getHost());
        $this->assertEquals('www.google.com:8080', $request->getHeader('Host'));
        $this->assertEquals('/path', $request->getPath());
        $this->assertEquals('/path?q=1&v=2', $request->getResource());
        $this->assertInstanceOf('Guzzle\\Http\\EntityBody', $request->getBody());
        $this->assertEquals('Data', (string) $request->getBody());
        $this->assertEquals('michael', $request->getUsername());
        $this->assertEquals('123', $request->getPassword());
        $this->assertEquals('8080', $request->getPort());
        $this->assertEquals(array(
            'scheme' => 'http',
            'host' => 'www.google.com',
            'port' => 8080,
            'path' => '/path',
            'query' => 'q=1&v=2',
        ), parse_url($request->getUrl()));
    }

    public function testCreatesFromMessage()
    {
        $auth = base64_encode('michael:123');
        $message = "PUT /path?q=1&v=2 HTTP/1.1\r\nHost: www.google.com:8080\r\nContent-Length: 4\r\nAuthorization: Basic {$auth}\r\n\r\nData";
        $request = RequestFactory::getInstance()->fromMessage($message);
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('PUT', $request->getMethod());
        $this->assertEquals('http', $request->getScheme());
        $this->assertEquals('http://www.google.com:8080/path?q=1&v=2', $request->getUrl());
        $this->assertEquals('www.google.com', $request->getHost());
        $this->assertEquals('www.google.com:8080', $request->getHeader('Host'));
        $this->assertEquals('/path', $request->getPath());
        $this->assertEquals('/path?q=1&v=2', $request->getResource());
        $this->assertInstanceOf('Guzzle\\Http\\EntityBody', $request->getBody());
        $this->assertEquals('Data', (string) $request->getBody());
        $this->assertEquals("Basic {$auth}", (string) $request->getHeader('Authorization'));
        $this->assertEquals('8080', $request->getPort());

        // Test passing a blank message returns false
        $this->assertFalse($request = RequestFactory::getInstance()->fromMessage(''));

        // Test passing a url with no port
        $message = "PUT /path?q=1&v=2 HTTP/1.1\r\nHost: www.google.com\r\nContent-Length: 4\r\nAuthorization: Basic {$auth}\r\n\r\nData";
        $request = RequestFactory::getInstance()->fromMessage($message);
        $this->assertInstanceOf('Guzzle\\Http\\Message\\EntityEnclosingRequest', $request);
        $this->assertEquals('PUT', $request->getMethod());
        $this->assertEquals('http', $request->getScheme());
        $this->assertEquals('http://www.google.com/path?q=1&v=2', $request->getUrl());
        $this->assertEquals('www.google.com', $request->getHost());
        $this->assertEquals('/path', $request->getPath());
        $this->assertEquals('/path?q=1&v=2', $request->getResource());
        $this->assertInstanceOf('Guzzle\\Http\\EntityBody', $request->getBody());
        $this->assertEquals('Data', (string) $request->getBody());
        $this->assertEquals("Basic {$auth}", (string) $request->getHeader('Authorization'));
        $this->assertEquals(80, $request->getPort());
    }

    public function testCreatesNewTraceRequest()
    {
        $request = RequestFactory::getInstance()->create('TRACE', 'http://www.google.com/');
        $this->assertFalse($request instanceof \Guzzle\Http\Message\EntityEnclosingRequest);
        $this->assertEquals('TRACE', $request->getMethod());
    }

    public function testCreatesProperTransferEncodingRequests()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.google.com/', array(
            'Transfer-Encoding' => 'chunked'
        ), 'hello');
        $this->assertEquals('chunked', $request->getHeader('Transfer-Encoding'));
        $this->assertFalse($request->hasHeader('Content-Length'));
    }

    public function testProperlyDealsWithDuplicateHeaders()
    {
        $parser = new MessageParser();

        $message = "POST / http/1.1\r\n"
            . "DATE:Mon, 09 Sep 2011 23:36:00 GMT\r\n"
            . "host:host.foo.com\r\n"
            . "ZOO:abc\r\n"
            . "ZOO:123\r\n"
            . "ZOO:HI\r\n"
            . "zoo:456\r\n\r\n";

        $parts = $parser->parseRequest($message);
        $this->assertEquals(array (
            'DATE' => 'Mon, 09 Sep 2011 23:36:00 GMT',
            'host' => 'host.foo.com',
            'ZOO'  => array('abc', '123', 'HI'),
            'zoo'  => '456',
        ), $parts['headers']);

        $request = RequestFactory::getInstance()->fromMessage($message);

        $this->assertEquals(array(
            'abc', '123', 'HI', '456'
        ), $request->getHeader('zoo')->toArray());
    }

    public function testCreatesHttpMessagesWithBodiesAndNormalizesLineEndings()
    {
        $message = "POST / http/1.1\r\n"
                 . "Content-Type:application/x-www-form-urlencoded; charset=utf8\r\n"
                 . "Date:Mon, 09 Sep 2011 23:36:00 GMT\r\n"
                 . "Host:host.foo.com\r\n\r\n"
                 . "foo=bar";

        $request = RequestFactory::getInstance()->fromMessage($message);
        $this->assertEquals('application/x-www-form-urlencoded; charset=utf8', (string) $request->getHeader('Content-Type'));
        $this->assertEquals('foo=bar', (string) $request->getBody());

        $message = "POST / http/1.1\n"
                 . "Content-Type:application/x-www-form-urlencoded; charset=utf8\n"
                 . "Date:Mon, 09 Sep 2011 23:36:00 GMT\n"
                 . "Host:host.foo.com\n\n"
                 . "foo=bar";
        $request = RequestFactory::getInstance()->fromMessage($message);
        $this->assertEquals('foo=bar', (string) $request->getBody());

        $message = "PUT / HTTP/1.1\r\nContent-Length: 0\r\n\r\n";
        $request = RequestFactory::getInstance()->fromMessage($message);
        $this->assertTrue($request->hasHeader('Content-Length'));
        $this->assertEquals(0, (string) $request->getHeader('Content-Length'));
    }

    public function testBugPathIncorrectlyHandled()
    {
        $message = "POST /foo\r\n\r\nBODY";
        $request = RequestFactory::getInstance()->fromMessage($message);
        $this->assertSame('POST', $request->getMethod());
        $this->assertSame('/foo', $request->getPath());
        $this->assertSame('BODY', (string) $request->getBody());
    }

    public function testHandlesChunkedTransferEncoding()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.foo.com/', array(
            'Transfer-Encoding' => 'chunked'
        ), 'Test');
        $this->assertFalse($request->hasHeader('Content-Length'));
        $this->assertEquals('chunked', $request->getHeader('Transfer-Encoding'));

        $request = RequestFactory::getInstance()->create('POST', 'http://www.foo.com/', array(
            'transfer-encoding' => 'chunked'
        ), array(
            'foo' => 'bar'
        ));

        $this->assertFalse($request->hasHeader('Content-Length'));
        $this->assertEquals('chunked', $request->getHeader('Transfer-Encoding'));
    }

    public function testClonesRequestsWithMethodWithoutClient()
    {
        $f = RequestFactory::getInstance();
        $request = $f->create('GET', 'http://www.test.com', array('X-Foo' => 'Bar'));
        $request->getParams()->replace(array('test' => '123'));
        $request->getCurlOptions()->set('foo', 'bar');
        $cloned = $f->cloneRequestWithMethod($request, 'PUT');
        $this->assertEquals('PUT', $cloned->getMethod());
        $this->assertEquals('Bar', (string) $cloned->getHeader('X-Foo'));
        $this->assertEquals('http://www.test.com', $cloned->getUrl());
        // Ensure params are cloned and cleaned up
        $this->assertEquals(1, count($cloned->getParams()->getAll()));
        $this->assertEquals('123', $cloned->getParams()->get('test'));
        // Ensure curl options are cloned
        $this->assertEquals('bar', $cloned->getCurlOptions()->get('foo'));
        // Ensure event dispatcher is cloned
        $this->assertNotSame($request->getEventDispatcher(), $cloned->getEventDispatcher());
    }

    public function testClonesRequestsWithMethodWithClient()
    {
        $f = RequestFactory::getInstance();
        $client = new Client();
        $request = $client->put('http://www.test.com', array('Content-Length' => 4), 'test');
        $cloned = $f->cloneRequestWithMethod($request, 'GET');
        $this->assertEquals('GET', $cloned->getMethod());
        $this->assertNull($cloned->getHeader('Content-Length'));
        $this->assertEquals('http://www.test.com', $cloned->getUrl());
        $this->assertSame($request->getClient(), $cloned->getClient());
    }

    public function testClonesRequestsWithMethodWithClientWithEntityEnclosingChange()
    {
        $f = RequestFactory::getInstance();
        $client = new Client();
        $request = $client->put('http://www.test.com', array('Content-Length' => 4), 'test');
        $cloned = $f->cloneRequestWithMethod($request, 'POST');
        $this->assertEquals('POST', $cloned->getMethod());
        $this->assertEquals('test', (string) $cloned->getBody());
    }

    public function testCanDisableRedirects()
    {
        $this->getServer()->enqueue(array(
            "HTTP/1.1 307\r\nLocation: " . $this->getServer()->getUrl() . "\r\nContent-Length: 0\r\n\r\n"
        ));
        $client = new Client($this->getServer()->getUrl());
        $response = $client->get('/', array(), array('allow_redirects' => false))->send();
        $this->assertEquals(307, $response->getStatusCode());
    }

    public function testCanAddCookies()
    {
        $client = new Client($this->getServer()->getUrl());
        $request = $client->get('/', array(), array('cookies' => array('Foo' => 'Bar')));
        $this->assertEquals('Bar', $request->getCookie('Foo'));
    }

    public function testCanAddQueryString()
    {
        $request = RequestFactory::getInstance()->create('GET', 'http://foo.com', array(), null, array(
            'query' => array('Foo' => 'Bar')
        ));
        $this->assertEquals('Bar', $request->getQuery()->get('Foo'));
    }

    public function testCanSetDefaultQueryString()
    {
        $request = new Request('GET', 'http://www.foo.com?test=abc');
        RequestFactory::getInstance()->applyOptions($request, array(
            'query' => array('test' => '123', 'other' => 't123')
        ), RequestFactory::OPTIONS_AS_DEFAULTS);
        $this->assertEquals('abc', $request->getQuery()->get('test'));
        $this->assertEquals('t123', $request->getQuery()->get('other'));
    }

    public function testCanAddBasicAuth()
    {
        $request = RequestFactory::getInstance()->create('GET', 'http://foo.com', array(), null, array(
            'auth' => array('michael', 'test')
        ));
        $this->assertEquals('michael', $request->getUsername());
        $this->assertEquals('test', $request->getPassword());
    }

    public function testCanAddDigestAuth()
    {
        $request = RequestFactory::getInstance()->create('GET', 'http://foo.com', array(), null, array(
            'auth' => array('michael', 'test', 'digest')
        ));
        $this->assertEquals(CURLAUTH_DIGEST, $request->getCurlOptions()->get(CURLOPT_HTTPAUTH));
        $this->assertEquals('michael', $request->getUsername());
        $this->assertEquals('test', $request->getPassword());
    }

    public function testCanAddEvents()
    {
        $foo = null;
        $client = new Client();
        $client->addSubscriber(new MockPlugin(array(new Response(200))));
        $request = $client->get($this->getServer()->getUrl(), array(), array(
            'events' => array(
                'request.before_send' => function () use (&$foo) { $foo = true; }
            )
        ));
        $request->send();
        $this->assertTrue($foo);
    }

    public function testCanAddEventsWithPriority()
    {
        $foo = null;
        $client = new Client();
        $client->addSubscriber(new MockPlugin(array(new Response(200))));
        $request = $client->get($this->getServer()->getUrl(), array(), array(
            'events' => array(
                'request.before_send' => array(function () use (&$foo) { $foo = true; }, 100)
            )
        ));
        $request->send();
        $this->assertTrue($foo);
    }

    public function testCanAddPlugins()
    {
        $mock = new MockPlugin(array(
            new Response(200),
            new Response(200)
        ));
        $client = new Client();
        $client->addSubscriber($mock);
        $request = $client->get('/', array(), array(
            'plugins' => array($mock)
        ));
        $request->send();
    }

    public function testCanDisableExceptions()
    {
        $client = new Client();
        $request = $client->get('/', array(), array(
            'plugins' => array(new MockPlugin(array(new Response(500)))),
            'exceptions' => false
        ));
        $this->assertEquals(500, $request->send()->getStatusCode());
    }

    public function testCanDisableExceptionsWithErrorListener()
    {
        $client = new Client();
        $client->getEventDispatcher()->addListener('request.error', function () {});
        $request = $client->get('/', array(), array(
                'plugins' => array(new MockPlugin(array(new Response(500)))),
                'exceptions' => false
            ));
        $this->assertEquals(500, $request->send()->getStatusCode());
    }

    public function testCanChangeSaveToLocation()
    {
        $r = EntityBody::factory();
        $client = new Client();
        $request = $client->get('/', array(), array(
            'plugins' => array(new MockPlugin(array(new Response(200, array(), 'testing')))),
            'save_to' => $r
        ));
        $request->send();
        $this->assertEquals('testing', (string) $r);
    }

    public function testCanSetProxy()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('proxy' => '192.168.16.121'));
        $this->assertEquals('192.168.16.121', $request->getCurlOptions()->get(CURLOPT_PROXY));
    }

    public function testCanSetHeadersOption()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('headers' => array('Foo' => 'Bar')));
        $this->assertEquals('Bar', (string) $request->getHeader('Foo'));
    }

    public function testCanSetDefaultHeadersOptions()
    {
        $request = new Request('GET', 'http://www.foo.com', array('Foo' => 'Bar'));
        RequestFactory::getInstance()->applyOptions($request, array(
            'headers' => array('Foo' => 'Baz', 'Bam' => 't123')
        ), RequestFactory::OPTIONS_AS_DEFAULTS);
        $this->assertEquals('Bar', (string) $request->getHeader('Foo'));
        $this->assertEquals('t123', (string) $request->getHeader('Bam'));
    }

    public function testCanSetBodyOption()
    {
        $client = new Client();
        $request = $client->put('/', array(), null, array('body' => 'test'));
        $this->assertEquals('test', (string) $request->getBody());
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testValidatesBodyOption()
    {
        $client = new Client();
        $client->get('/', array(), array('body' => 'test'));
    }

    public function testCanSetTimeoutOption()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('timeout' => 1.5));
        $this->assertEquals(1500, $request->getCurlOptions()->get(CURLOPT_TIMEOUT_MS));
    }

    public function testCanSetConnectTimeoutOption()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('connect_timeout' => 1.5));
        $this->assertEquals(1500, $request->getCurlOptions()->get(CURLOPT_CONNECTTIMEOUT_MS));
    }

    public function testCanSetDebug()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('debug' => true));
        $this->assertTrue($request->getCurlOptions()->get(CURLOPT_VERBOSE));
    }

    public function testCanSetVerifyToOff()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('verify' => false));
        $this->assertNull($request->getCurlOptions()->get(CURLOPT_CAINFO));
        $this->assertSame(0, $request->getCurlOptions()->get(CURLOPT_SSL_VERIFYHOST));
        $this->assertFalse($request->getCurlOptions()->get(CURLOPT_SSL_VERIFYPEER));
    }

    public function testCanSetVerifyToOn()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('verify' => true));
        $this->assertNotNull($request->getCurlOptions()->get(CURLOPT_CAINFO));
        $this->assertSame(2, $request->getCurlOptions()->get(CURLOPT_SSL_VERIFYHOST));
        $this->assertTrue($request->getCurlOptions()->get(CURLOPT_SSL_VERIFYPEER));
    }

    public function testCanSetVerifyToPath()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('verify' => '/foo.pem'));
        $this->assertEquals('/foo.pem', $request->getCurlOptions()->get(CURLOPT_CAINFO));
        $this->assertSame(2, $request->getCurlOptions()->get(CURLOPT_SSL_VERIFYHOST));
        $this->assertTrue($request->getCurlOptions()->get(CURLOPT_SSL_VERIFYPEER));
    }

    public function inputValidation()
    {
        return array_map(function ($option) { return array($option); }, array(
            'headers', 'query', 'cookies', 'auth', 'events', 'plugins', 'params'
        ));
    }

    /**
     * @dataProvider inputValidation
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testValidatesInput($option)
    {
        $client = new Client();
        $client->get('/', array(), array($option => 'foo'));
    }

    public function testCanAddRequestParams()
    {
        $client = new Client();
        $request = $client->put('/', array(), null, array('params' => array('foo' => 'test')));
        $this->assertEquals('test', $request->getParams()->get('foo'));
    }

    public function testCanAddSslKey()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('ssl_key' => '/foo.pem'));
        $this->assertEquals('/foo.pem', $request->getCurlOptions()->get(CURLOPT_SSLKEY));
    }

    public function testCanAddSslKeyPassword()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('ssl_key' => array('/foo.pem', 'bar')));
        $this->assertEquals('/foo.pem', $request->getCurlOptions()->get(CURLOPT_SSLKEY));
        $this->assertEquals('bar', $request->getCurlOptions()->get(CURLOPT_SSLKEYPASSWD));
    }

    public function testCanAddSslCert()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('cert' => '/foo.pem'));
        $this->assertEquals('/foo.pem', $request->getCurlOptions()->get(CURLOPT_SSLCERT));
    }

    public function testCanAddSslCertPassword()
    {
        $client = new Client();
        $request = $client->get('/', array(), array('cert' => array('/foo.pem', 'bar')));
        $this->assertEquals('/foo.pem', $request->getCurlOptions()->get(CURLOPT_SSLCERT));
        $this->assertEquals('bar', $request->getCurlOptions()->get(CURLOPT_SSLCERTPASSWD));
    }

    public function testCreatesBodyWithoutZeroString()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://test.com', array(), '0');
        $this->assertSame('0', (string) $request->getBody());
    }
}
