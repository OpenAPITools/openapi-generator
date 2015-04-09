<?php

namespace Guzzle\Tests\Http\Curl;

use Guzzle\Http\Client;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Curl\CurlMultiProxy;

/**
 * @group server
 * @covers Guzzle\Http\Curl\CurlMultiProxy
 */
class CurlMultiProxyTest extends \Guzzle\Tests\GuzzleTestCase
{
    const SELECT_TIMEOUT = 23.1;

    const MAX_HANDLES = 2;

    /** @var \Guzzle\Http\Curl\CurlMultiProxy */
    private $multi;

    protected function setUp()
    {
        parent::setUp();
        $this->multi = new CurlMultiProxy(self::MAX_HANDLES, self::SELECT_TIMEOUT);
    }

    public function tearDown()
    {
        unset($this->multi);
    }

    public function testConstructorSetsMaxHandles()
    {
        $m = new CurlMultiProxy(self::MAX_HANDLES, self::SELECT_TIMEOUT);
        $this->assertEquals(self::MAX_HANDLES, $this->readAttribute($m, 'maxHandles'));
    }

    public function testConstructorSetsSelectTimeout()
    {
        $m = new CurlMultiProxy(self::MAX_HANDLES, self::SELECT_TIMEOUT);
        $this->assertEquals(self::SELECT_TIMEOUT, $this->readAttribute($m, 'selectTimeout'));
    }

    public function testAddingRequestsAddsToQueue()
    {
        $r = new Request('GET', 'http://www.foo.com');
        $this->assertSame($this->multi, $this->multi->add($r));
        $this->assertEquals(1, count($this->multi));
        $this->assertEquals(array($r), $this->multi->all());

        $this->assertTrue($this->multi->remove($r));
        $this->assertFalse($this->multi->remove($r));
        $this->assertEquals(0, count($this->multi));
    }

    public function testResetClearsState()
    {
        $r = new Request('GET', 'http://www.foo.com');
        $this->multi->add($r);
        $this->multi->reset();
        $this->assertEquals(0, count($this->multi));
    }

    public function testSendWillSendQueuedRequestsFirst()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
        ));
        $client = new Client($this->getServer()->getUrl());
        $events = array();
        $client->getCurlMulti()->getEventDispatcher()->addListener(
            CurlMultiProxy::ADD_REQUEST,
            function ($e) use (&$events) {
                $events[] = $e;
            }
        );
        $request = $client->get();
        $request->getEventDispatcher()->addListener('request.complete', function () use ($client) {
            $client->get('/foo')->send();
        });
        $request->send();
        $received = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals(2, count($received));
        $this->assertEquals($this->getServer()->getUrl(), $received[0]->getUrl());
        $this->assertEquals($this->getServer()->getUrl() . 'foo', $received[1]->getUrl());
        $this->assertEquals(2, count($events));
    }

    public function testTrimsDownMaxHandleCount()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 307 OK\r\nLocation: /foo\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 307 OK\r\nLocation: /foo\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 307 OK\r\nLocation: /foo\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 307 OK\r\nLocation: /foo\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
        ));
        $client = new Client($this->getServer()->getUrl());
        $client->setCurlMulti(new CurlMultiProxy(self::MAX_HANDLES, self::SELECT_TIMEOUT));
        $request = $client->get();
        $request->send();
        $this->assertEquals(200, $request->getResponse()->getStatusCode());
        $handles = $this->readAttribute($client->getCurlMulti(), 'handles');
        $this->assertEquals(2, count($handles));
    }
}
