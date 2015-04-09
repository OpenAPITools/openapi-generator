<?php

namespace Guzzle\Tests\Stream;

use Guzzle\Stream\Stream;
use Guzzle\Stream\PhpStreamRequestFactory;
use Guzzle\Http\Client;

/**
 * @group server
 * @covers \Guzzle\Stream\PhpStreamRequestFactory
 */
class PhpStreamRequestFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var Client */
    protected $client;

    /** @var PhpStreamRequestFactory */
    protected $factory;

    protected function setUp()
    {
        $this->client = new Client($this->getServer()->getUrl());
        $this->factory = new PhpStreamRequestFactory();
    }

    public function testOpensValidStreamByCreatingContext()
    {
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi");
        $request = $this->client->get('/');
        $stream = $this->factory->fromRequest($request);
        $this->assertEquals('hi', (string) $stream);
        $headers = $this->factory->getLastResponseHeaders();
        $this->assertContains('HTTP/1.1 200 OK', $headers);
        $this->assertContains('Content-Length: 2', $headers);
        $this->assertSame($headers, $stream->getCustomData('response_headers'));
        $this->assertEquals(2, $stream->getSize());
    }

    public function testOpensValidStreamByPassingContextAndMerging()
    {
        $request = $this->client->get('/');
        $this->factory = $this->getMockBuilder('Guzzle\Stream\PhpStreamRequestFactory')
            ->setMethods(array('createContext', 'createStream'))
            ->getMock();
        $this->factory->expects($this->never())
            ->method('createContext');
        $this->factory->expects($this->once())
            ->method('createStream')
            ->will($this->returnValue(new Stream(fopen('php://temp', 'r'))));

        $context = array('http' => array('method' => 'HEAD', 'ignore_errors' => false));
        $this->factory->fromRequest($request, stream_context_create($context));
        $options = stream_context_get_options($this->readAttribute($this->factory, 'context'));
        $this->assertEquals('HEAD', $options['http']['method']);
        $this->assertFalse($options['http']['ignore_errors']);
        $this->assertEquals('1.1', $options['http']['protocol_version']);
    }

    public function testAppliesProxySettings()
    {
        $request = $this->client->get('/');
        $request->getCurlOptions()->set(CURLOPT_PROXY, 'tcp://foo.com');
        $this->factory = $this->getMockBuilder('Guzzle\Stream\PhpStreamRequestFactory')
            ->setMethods(array('createStream'))
            ->getMock();
        $this->factory->expects($this->once())
            ->method('createStream')
            ->will($this->returnValue(new Stream(fopen('php://temp', 'r'))));
        $this->factory->fromRequest($request);
        $options = stream_context_get_options($this->readAttribute($this->factory, 'context'));
        $this->assertEquals('tcp://foo.com', $options['http']['proxy']);
    }

    public function testAddsPostFields()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi");
        $request = $this->client->post('/', array('Foo' => 'Bar'), array('foo' => 'baz bar'));
        $stream = $this->factory->fromRequest($request);
        $this->assertEquals('hi', (string) $stream);

        $headers = $this->factory->getLastResponseHeaders();
        $this->assertContains('HTTP/1.1 200 OK', $headers);
        $this->assertContains('Content-Length: 2', $headers);
        $this->assertSame($headers, $stream->getCustomData('response_headers'));

        $received = $this->getServer()->getReceivedRequests();
        $this->assertEquals(1, count($received));
        $this->assertContains('POST / HTTP/1.1', $received[0]);
        $this->assertContains('host: ', $received[0]);
        $this->assertContains('user-agent: Guzzle/', $received[0]);
        $this->assertContains('foo: Bar', $received[0]);
        $this->assertContains('content-length: 13', $received[0]);
        $this->assertContains('foo=baz%20bar', $received[0]);
    }

    public function testAddsBody()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi");
        $request = $this->client->put('/', array('Foo' => 'Bar'), 'Testing...123');
        $stream = $this->factory->fromRequest($request);
        $this->assertEquals('hi', (string) $stream);

        $headers = $this->factory->getLastResponseHeaders();
        $this->assertContains('HTTP/1.1 200 OK', $headers);
        $this->assertContains('Content-Length: 2', $headers);
        $this->assertSame($headers, $stream->getCustomData('response_headers'));

        $received = $this->getServer()->getReceivedRequests();
        $this->assertEquals(1, count($received));
        $this->assertContains('PUT / HTTP/1.1', $received[0]);
        $this->assertContains('host: ', $received[0]);
        $this->assertContains('user-agent: Guzzle/', $received[0]);
        $this->assertContains('foo: Bar', $received[0]);
        $this->assertContains('content-length: 13', $received[0]);
        $this->assertContains('Testing...123', $received[0]);
    }

    public function testCanDisableSslValidation()
    {
        $request = $this->client->get('/');
        $request->getCurlOptions()->set(CURLOPT_SSL_VERIFYPEER, false);
        $this->factory = $this->getMockBuilder('Guzzle\Stream\PhpStreamRequestFactory')
            ->setMethods(array('createStream'))
            ->getMock();
        $this->factory->expects($this->once())
            ->method('createStream')
            ->will($this->returnValue(new Stream(fopen('php://temp', 'r'))));
        $this->factory->fromRequest($request);
        $options = stream_context_get_options($this->readAttribute($this->factory, 'context'));
        $this->assertFalse($options['ssl']['verify_peer']);
    }

    public function testUsesSslValidationByDefault()
    {
        $request = $this->client->get('/');
        $this->factory = $this->getMockBuilder('Guzzle\Stream\PhpStreamRequestFactory')
            ->setMethods(array('createStream'))
            ->getMock();
        $this->factory->expects($this->once())
            ->method('createStream')
            ->will($this->returnValue(new Stream(fopen('php://temp', 'r'))));
        $this->factory->fromRequest($request);
        $options = stream_context_get_options($this->readAttribute($this->factory, 'context'));
        $this->assertTrue($options['ssl']['verify_peer']);
        $this->assertSame($request->getCurlOptions()->get(CURLOPT_CAINFO), $options['ssl']['cafile']);
    }

    public function testBasicAuthAddsUserAndPassToUrl()
    {
        $request = $this->client->get('/');
        $request->setAuth('Foo', 'Bar');
        $this->factory = $this->getMockBuilder('Guzzle\Stream\PhpStreamRequestFactory')
            ->setMethods(array('createStream'))
            ->getMock();
        $this->factory->expects($this->once())
            ->method('createStream')
            ->will($this->returnValue(new Stream(fopen('php://temp', 'r'))));
        $this->factory->fromRequest($request);
        $this->assertContains('Foo:Bar@', (string) $this->readAttribute($this->factory, 'url'));
    }

    public function testCanCreateCustomStreamClass()
    {
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi");
        $request = $this->client->get('/');
        $stream = $this->factory->fromRequest($request, array(), array('stream_class' => 'Guzzle\Http\EntityBody'));
        $this->assertInstanceOf('Guzzle\Http\EntityBody', $stream);
    }
}
