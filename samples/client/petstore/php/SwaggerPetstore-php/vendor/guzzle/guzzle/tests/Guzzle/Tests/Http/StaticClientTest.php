<?php

namespace Guzzle\Tests\Plugin\Redirect;

use Guzzle\Http\Client;
use Guzzle\Http\StaticClient;
use Guzzle\Plugin\Mock\MockPlugin;
use Guzzle\Http\Message\Response;
use Guzzle\Stream\Stream;

/**
 * @covers Guzzle\Http\StaticClient
 */
class StaticClientTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testMountsClient()
    {
        $client = new Client();
        StaticClient::mount('FooBazBar', $client);
        $this->assertTrue(class_exists('FooBazBar'));
        $this->assertSame($client, $this->readAttribute('Guzzle\Http\StaticClient', 'client'));
    }

    public function requestProvider()
    {
        return array_map(
            function ($m) { return array($m); },
            array('GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS')
        );
    }

    /**
     * @dataProvider requestProvider
     */
    public function testSendsRequests($method)
    {
        $mock = new MockPlugin(array(new Response(200)));
        call_user_func('Guzzle\Http\StaticClient::' . $method, 'http://foo.com', array(
            'plugins' => array($mock)
        ));
        $requests = $mock->getReceivedRequests();
        $this->assertCount(1, $requests);
        $this->assertEquals($method, $requests[0]->getMethod());
    }

    public function testCanCreateStreamsUsingDefaultFactory()
    {
        $this->getServer()->enqueue(array("HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ntest"));
        $stream = StaticClient::get($this->getServer()->getUrl(), array('stream' => true));
        $this->assertInstanceOf('Guzzle\Stream\StreamInterface', $stream);
        $this->assertEquals('test', (string) $stream);
    }

    public function testCanCreateStreamsUsingCustomFactory()
    {
        $stream = $this->getMockBuilder('Guzzle\Stream\StreamRequestFactoryInterface')
            ->setMethods(array('fromRequest'))
            ->getMockForAbstractClass();
        $resource = new Stream(fopen('php://temp', 'r+'));
        $stream->expects($this->once())
            ->method('fromRequest')
            ->will($this->returnValue($resource));
        $this->getServer()->enqueue(array("HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ntest"));
        $result = StaticClient::get($this->getServer()->getUrl(), array('stream' => $stream));
        $this->assertSame($resource, $result);
    }
}
