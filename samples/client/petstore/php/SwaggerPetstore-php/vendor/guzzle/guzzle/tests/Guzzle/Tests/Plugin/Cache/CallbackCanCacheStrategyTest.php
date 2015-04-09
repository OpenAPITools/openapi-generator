<?php

namespace Guzzle\Tests\Plugin\Cache;

use Doctrine\Common\Cache\ArrayCache;
use Guzzle\Cache\DoctrineCacheAdapter;
use Guzzle\Common\Event;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Cache\CachePlugin;
use Guzzle\Plugin\Cache\CallbackCanCacheStrategy;

/**
 * @covers Guzzle\Plugin\Cache\CallbackCanCacheStrategy
 */
class CallbackCanCacheStrategyTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testConstructorEnsuresCallbackIsCallable()
    {
        $p = new CallbackCanCacheStrategy(new \stdClass());
    }

    public function testUsesCallback()
    {
        $c = new CallbackCanCacheStrategy(function ($request) { return true; });
        $this->assertTrue($c->canCacheRequest(new Request('DELETE', 'http://www.foo.com')));
    }

    /**
     * The following is a bit of an integration test to ensure that the CachePlugin honors a
     * custom can cache strategy.
     */
    public function testIntegrationWithCachePlugin()
    {
        $c = new CallbackCanCacheStrategy(
            function ($request) { return true; },
            function ($response) { return true; }
        );

        // Make a request and response that have no business being cached
        $request = new Request('DELETE', 'http://www.foo.com');
        $response = Response::fromMessage(
            "HTTP/1.1 200 OK\r\n"
            . "Expires: Mon, 26 Jul 1997 05:00:00 GMT\r\n"
            . "Last-Modified: Wed, 09 Jan 2013 08:48:53 GMT\r\n"
            . "Content-Length: 2\r\n"
            . "Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0\r\n\r\n"
            . "hi"
        );

        $this->assertTrue($c->canCacheRequest($request));
        $this->assertTrue($c->canCacheResponse($response));

        $s = $this->getMockBuilder('Guzzle\Plugin\Cache\DefaultCacheStorage')
            ->setConstructorArgs(array(new DoctrineCacheAdapter(new ArrayCache())))
            ->setMethods(array('fetch'))
            ->getMockForAbstractClass();

        $s->expects($this->once())
            ->method('fetch')
            ->will($this->returnValue($response));

        $plugin = new CachePlugin(array('can_cache' => $c, 'storage' => $s));
        $plugin->onRequestBeforeSend(new Event(array('request' => $request)));

        $this->assertEquals(200, $request->getResponse()->getStatusCode());
        $this->assertEquals('hi', $request->getResponse()->getBody(true));
    }
}
