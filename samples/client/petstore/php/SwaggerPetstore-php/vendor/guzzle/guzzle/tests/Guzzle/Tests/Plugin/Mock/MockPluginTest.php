<?php

namespace Guzzle\Tests\Plugin\Mock;

use Guzzle\Common\Event;
use Guzzle\Http\EntityBody;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Mock\MockPlugin;
use Guzzle\Http\Client;
use Guzzle\Http\Exception\CurlException;

/**
 * @covers Guzzle\Plugin\Mock\MockPlugin
 */
class MockPluginTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testDescribesSubscribedEvents()
    {
        $this->assertInternalType('array', MockPlugin::getSubscribedEvents());
    }

    public function testDescribesEvents()
    {
        $this->assertInternalType('array', MockPlugin::getAllEvents());
    }

    public function testCanBeTemporary()
    {
        $plugin = new MockPlugin();
        $this->assertFalse($plugin->isTemporary());
        $plugin = new MockPlugin(null, true);
        $this->assertTrue($plugin->isTemporary());
    }

    public function testIsCountable()
    {
        $plugin = new MockPlugin();
        $plugin->addResponse(Response::fromMessage("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"));
        $this->assertEquals(1, count($plugin));
    }

    /**
     * @depends testIsCountable
     */
    public function testCanClearQueue()
    {
        $plugin = new MockPlugin();
        $plugin->addResponse(Response::fromMessage("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"));
        $plugin->clearQueue();
        $this->assertEquals(0, count($plugin));
    }

    public function testCanInspectQueue()
    {
        $plugin = new MockPlugin();
        $this->assertInternalType('array', $plugin->getQueue());
        $plugin->addResponse(Response::fromMessage("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"));
        $queue = $plugin->getQueue();
        $this->assertInternalType('array', $queue);
        $this->assertEquals(1, count($queue));
    }

    public function testRetrievesResponsesFromFiles()
    {
        $response = MockPlugin::getMockFile(__DIR__ . '/../../TestData/mock_response');
        $this->assertInstanceOf('Guzzle\\Http\\Message\\Response', $response);
        $this->assertEquals(200, $response->getStatusCode());
    }

    /**
     * @expectedException InvalidArgumentException
     */
    public function testThrowsExceptionWhenResponseFileIsNotFound()
    {
        MockPlugin::getMockFile('missing/filename');
    }

    /**
     * @expectedException InvalidArgumentException
     */
    public function testInvalidResponsesThrowAnException()
    {
        $p = new MockPlugin();
        $p->addResponse($this);
    }

    public function testAddsResponseObjectsToQueue()
    {
        $p = new MockPlugin();
        $response = Response::fromMessage("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $p->addResponse($response);
        $this->assertEquals(array($response), $p->getQueue());
    }

    public function testAddsResponseFilesToQueue()
    {
        $p = new MockPlugin();
        $p->addResponse(__DIR__ . '/../../TestData/mock_response');
        $this->assertEquals(1, count($p));
    }

    /**
     * @depends testAddsResponseFilesToQueue
     */
    public function testAddsMockResponseToRequestFromClient()
    {
        $p = new MockPlugin();
        $response = MockPlugin::getMockFile(__DIR__ . '/../../TestData/mock_response');
        $p->addResponse($response);

        $client = new Client('http://127.0.0.1:123/');
        $client->getEventDispatcher()->addSubscriber($p, 9999);
        $request = $client->get();
        $request->send();

        $this->assertSame($response, $request->getResponse());
        $this->assertEquals(0, count($p));
    }

    /**
     * @depends testAddsResponseFilesToQueue
     * @expectedException \OutOfBoundsException
     */
    public function testUpdateThrowsExceptionWhenEmpty()
    {
        $p = new MockPlugin();
        $p->onRequestBeforeSend(new Event());
    }

    /**
     * @depends testAddsMockResponseToRequestFromClient
     */
    public function testDetachesTemporaryWhenEmpty()
    {
        $p = new MockPlugin(null, true);
        $p->addResponse(MockPlugin::getMockFile(__DIR__ . '/../../TestData/mock_response'));
        $client = new Client('http://127.0.0.1:123/');
        $client->getEventDispatcher()->addSubscriber($p, 9999);
        $request = $client->get();
        $request->send();

        $this->assertFalse($this->hasSubscriber($client, $p));
    }

    public function testLoadsResponsesFromConstructor()
    {
        $p = new MockPlugin(array(new Response(200)));
        $this->assertEquals(1, $p->count());
    }

    public function testStoresMockedRequests()
    {
        $p = new MockPlugin(array(new Response(200), new Response(200)));
        $client = new Client('http://127.0.0.1:123/');
        $client->getEventDispatcher()->addSubscriber($p, 9999);

        $request1 = $client->get();
        $request1->send();
        $this->assertEquals(array($request1), $p->getReceivedRequests());

        $request2 = $client->get();
        $request2->send();
        $this->assertEquals(array($request1, $request2), $p->getReceivedRequests());

        $p->flush();
        $this->assertEquals(array(), $p->getReceivedRequests());
    }

    public function testReadsBodiesFromMockedRequests()
    {
        $p = new MockPlugin(array(new Response(200)));
        $p->readBodies(true);
        $client = new Client('http://127.0.0.1:123/');
        $client->getEventDispatcher()->addSubscriber($p, 9999);

        $body = EntityBody::factory('foo');
        $request = $client->put();
        $request->setBody($body);
        $request->send();
        $this->assertEquals(3, $body->ftell());
    }

    public function testCanMockBadRequestExceptions()
    {
        $client = new Client('http://127.0.0.1:123/');
        $ex = new CurlException('Foo');
        $mock = new MockPlugin(array($ex));
        $client->addSubscriber($mock);
        $request = $client->get('foo');

        try {
            $request->send();
            $this->fail('Did not dequeue an exception');
        } catch (CurlException $e) {
            $this->assertSame($e, $ex);
            $this->assertSame($request, $ex->getRequest());
        }
    }
}
