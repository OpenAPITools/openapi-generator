<?php

namespace Guzzle\Tests\Http\Curl;

use Guzzle\Http\Client;
use Guzzle\Http\Message\EntityEnclosingRequest;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Curl\RequestMediator;

/**
 * @covers Guzzle\Http\Curl\RequestMediator
 */
class RequestMediatorTest extends \Guzzle\Tests\GuzzleTestCase
{
    public $events = array();

    public function event($event)
    {
        $this->events[] = $event;
    }

    public function testEmitsEvents()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://www.example.com');
        $request->setBody('foo');
        $request->setResponse(new Response(200));

        // Ensure that IO events are emitted
        $request->getCurlOptions()->set('emit_io', true);

        // Attach listeners for each event type
        $request->getEventDispatcher()->addListener('curl.callback.progress', array($this, 'event'));
        $request->getEventDispatcher()->addListener('curl.callback.read', array($this, 'event'));
        $request->getEventDispatcher()->addListener('curl.callback.write', array($this, 'event'));

        $mediator = new RequestMediator($request, true);

        $mediator->progress('a', 'b', 'c', 'd');
        $this->assertEquals(1, count($this->events));
        $this->assertEquals('curl.callback.progress', $this->events[0]->getName());

        $this->assertEquals(3, $mediator->writeResponseBody('foo', 'bar'));
        $this->assertEquals(2, count($this->events));
        $this->assertEquals('curl.callback.write', $this->events[1]->getName());
        $this->assertEquals('bar', $this->events[1]['write']);
        $this->assertSame($request, $this->events[1]['request']);

        $this->assertEquals('foo', $mediator->readRequestBody('a', 'b', 3));
        $this->assertEquals(3, count($this->events));
        $this->assertEquals('curl.callback.read', $this->events[2]->getName());
        $this->assertEquals('foo', $this->events[2]['read']);
        $this->assertSame($request, $this->events[2]['request']);
    }

    public function testDoesNotUseRequestResponseBodyWhenNotCustom()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 307 Foo\r\nLocation: /foo\r\nContent-Length: 2\r\n\r\nHI",
            "HTTP/1.1 301 Foo\r\nLocation: /foo\r\nContent-Length: 2\r\n\r\nFI",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ntest",
        ));
        $client = new Client($this->getServer()->getUrl());
        $response = $client->get()->send();
        $this->assertEquals('test', $response->getBody(true));
    }
}
