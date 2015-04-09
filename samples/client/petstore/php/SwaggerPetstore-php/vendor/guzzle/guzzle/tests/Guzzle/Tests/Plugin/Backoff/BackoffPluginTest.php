<?php

namespace Guzzle\Tests\Plugin\Backoff;

use Guzzle\Common\Event;
use Guzzle\Http\Exception\CurlException;
use Guzzle\Http\Client;
use Guzzle\Plugin\Backoff\BackoffPlugin;
use Guzzle\Http\Message\RequestInterface;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\EntityEnclosingRequest;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Curl\CurlMulti;
use Guzzle\Http\Curl\CurlMultiInterface;
use Guzzle\Plugin\Backoff\ConstantBackoffStrategy;
use Guzzle\Plugin\Backoff\CurlBackoffStrategy;
use Guzzle\Plugin\Backoff\HttpBackoffStrategy;
use Guzzle\Plugin\Backoff\TruncatedBackoffStrategy;
use Symfony\Component\EventDispatcher\EventSubscriberInterface;

/**
 * @group server
 * @covers Guzzle\Plugin\Backoff\BackoffPlugin
 */
class BackoffPluginTest extends \Guzzle\Tests\GuzzleTestCase implements EventSubscriberInterface
{
    protected $retried;

    public function setUp()
    {
        $this->retried = false;
    }

    public static function getSubscribedEvents()
    {
        return array(BackoffPlugin::RETRY_EVENT => 'onRequestRetry');
    }

    public function onRequestRetry(Event $event)
    {
        $this->retried = $event;
    }

    public function testHasEventList()
    {
        $this->assertEquals(1, count(BackoffPlugin::getAllEvents()));
    }

    public function testCreatesDefaultExponentialBackoffPlugin()
    {
        $plugin = BackoffPlugin::getExponentialBackoff(3, array(204), array(10));
        $this->assertInstanceOf('Guzzle\Plugin\Backoff\BackoffPlugin', $plugin);
        $strategy = $this->readAttribute($plugin, 'strategy');
        $this->assertInstanceOf('Guzzle\Plugin\Backoff\TruncatedBackoffStrategy', $strategy);
        $this->assertEquals(3, $this->readAttribute($strategy, 'max'));
        $strategy = $this->readAttribute($strategy, 'next');
        $this->assertInstanceOf('Guzzle\Plugin\Backoff\HttpBackoffStrategy', $strategy);
        $this->assertEquals(array(204 => true), $this->readAttribute($strategy, 'errorCodes'));
        $strategy = $this->readAttribute($strategy, 'next');
        $this->assertInstanceOf('Guzzle\Plugin\Backoff\CurlBackoffStrategy', $strategy);
        $this->assertEquals(array(10 => true), $this->readAttribute($strategy, 'errorCodes'));
        $strategy = $this->readAttribute($strategy, 'next');
        $this->assertInstanceOf('Guzzle\Plugin\Backoff\ExponentialBackoffStrategy', $strategy);
    }

    public function testDoesNotRetryUnlessStrategyReturnsNumber()
    {
        $request = new Request('GET', 'http://www.example.com');
        $request->setState('transfer');

        $mock = $this->getMockBuilder('Guzzle\Plugin\Backoff\BackoffStrategyInterface')
            ->setMethods(array('getBackoffPeriod'))
            ->getMockForAbstractClass();

        $mock->expects($this->once())
            ->method('getBackoffPeriod')
            ->will($this->returnValue(false));

        $plugin = new BackoffPlugin($mock);
        $plugin->addSubscriber($this);
        $plugin->onRequestSent(new Event(array('request' => $request)));
        $this->assertFalse($this->retried);
    }

    public function testUpdatesRequestForRetry()
    {
        $request = new Request('GET', 'http://www.example.com');
        $request->setState('transfer');
        $response = new Response(500);
        $handle = $this->getMockBuilder('Guzzle\Http\Curl\CurlHandle')->disableOriginalConstructor()->getMock();
        $e = new CurlException();
        $e->setCurlHandle($handle);

        $plugin = new BackoffPlugin(new ConstantBackoffStrategy(10));
        $plugin->addSubscriber($this);

        $event = new Event(array(
            'request'   => $request,
            'response'  => $response,
            'exception' => $e
        ));

        $plugin->onRequestSent($event);
        $this->assertEquals(array(
            'request'  => $request,
            'response' => $response,
            'handle'   => $handle,
            'retries'  => 1,
            'delay'    => 10
        ), $this->readAttribute($this->retried, 'context'));

        $plugin->onRequestSent($event);
        $this->assertEquals(array(
            'request'  => $request,
            'response' => $response,
            'handle'   => $handle,
            'retries'  => 2,
            'delay'    => 10
        ), $this->readAttribute($this->retried, 'context'));
    }

    public function testDoesNothingWhenNotRetryingAndPollingRequest()
    {
        $request = new Request('GET', 'http://www.foo.com');
        $plugin = new BackoffPlugin(new ConstantBackoffStrategy(10));
        $plugin->onRequestPoll(new Event(array('request' => $request)));
    }

    public function testRetriesRequests()
    {
        // Create a script to return several 500 and 503 response codes
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata"
        ));

        $plugin = new BackoffPlugin(
            new TruncatedBackoffStrategy(3,
                new HttpBackoffStrategy(null,
                    new CurlBackoffStrategy(null,
                        new ConstantBackoffStrategy(0.05)
                    )
                )
            )
        );

        $client = new Client($this->getServer()->getUrl());
        $client->getEventDispatcher()->addSubscriber($plugin);
        $request = $client->get();
        $request->send();

        // Make sure it eventually completed successfully
        $this->assertEquals(200, $request->getResponse()->getStatusCode());
        $this->assertEquals('data', $request->getResponse()->getBody(true));

        // Check that three requests were made to retry this request
        $this->assertEquals(3, count($this->getServer()->getReceivedRequests(false)));
        $this->assertEquals(2, $request->getParams()->get(BackoffPlugin::RETRY_PARAM));
    }

    /**
     * @expectedException \Guzzle\Http\Exception\ServerErrorResponseException
     */
    public function testFailsOnTruncation()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n"
        ));

        $plugin = new BackoffPlugin(
            new TruncatedBackoffStrategy(2,
                new HttpBackoffStrategy(null,
                    new ConstantBackoffStrategy(0.05)
                )
            )
        );

        $client = new Client($this->getServer()->getUrl());
        $client->addSubscriber($plugin);
        $client->get()->send();
    }

    public function testRetriesRequestsWhenInParallel()
    {
        // Create a script to return several 500 and 503 response codes
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata"
        ));

        $plugin = new BackoffPlugin(
            new HttpBackoffStrategy(null,
                new TruncatedBackoffStrategy(3,
                    new CurlBackoffStrategy(null,
                        new ConstantBackoffStrategy(0.1)
                    )
                )
            )
        );
        $client = new Client($this->getServer()->getUrl());
        $client->getEventDispatcher()->addSubscriber($plugin);
        $requests = array();
        for ($i = 0; $i < 5; $i++) {
            $requests[] = $client->get();
        }
        $client->send($requests);

        $this->assertEquals(15, count($this->getServer()->getReceivedRequests(false)));
    }

    /**
     * @covers Guzzle\Plugin\Backoff\BackoffPlugin
     * @covers Guzzle\Http\Curl\CurlMulti
     */
    public function testRetriesPooledRequestsUsingDelayAndPollingEvent()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\ndata"
        ));
        // Need to sleep for some time ensure that the polling works correctly in the observer
        $plugin = new BackoffPlugin(new HttpBackoffStrategy(null,
            new TruncatedBackoffStrategy(1,
                new ConstantBackoffStrategy(0.5))));

        $client = new Client($this->getServer()->getUrl());
        $client->getEventDispatcher()->addSubscriber($plugin);
        $request = $client->get();
        $request->send();
        // Make sure it eventually completed successfully
        $this->assertEquals('data', $request->getResponse()->getBody(true));
        // Check that two requests were made to retry this request
        $this->assertEquals(2, count($this->getServer()->getReceivedRequests(false)));
    }

    public function testSeeksToBeginningOfRequestBodyWhenRetrying()
    {
        // Create a request with a body
        $request = new EntityEnclosingRequest('PUT', 'http://www.example.com');
        $request->setBody('abc');
        // Set the retry time to be something that will be retried always
        $request->getParams()->set(BackoffPlugin::DELAY_PARAM, 2);
        // Seek to the end of the stream
        $request->getBody()->seek(3);
        $this->assertEquals('', $request->getBody()->read(1));
        // Create a plugin that does not delay when retrying
        $plugin = new BackoffPlugin(new ConstantBackoffStrategy(0));
        $plugin->onRequestPoll($this->getMockEvent($request));
        // Ensure that the stream was seeked to 0
        $this->assertEquals('a', $request->getBody()->read(1));
    }

    public function testDoesNotSeekOnRequestsWithNoBodyWhenRetrying()
    {
        // Create a request with a body
        $request = new EntityEnclosingRequest('PUT', 'http://www.example.com');
        $request->getParams()->set(BackoffPlugin::DELAY_PARAM, 2);
        $plugin = new BackoffPlugin(new ConstantBackoffStrategy(0));
        $plugin->onRequestPoll($this->getMockEvent($request));
    }

    protected function getMockEvent(RequestInterface $request)
    {
        // Create a mock curl multi object
        $multi = $this->getMockBuilder('Guzzle\Http\Curl\CurlMulti')
            ->setMethods(array('remove', 'add'))
            ->getMock();

        // Create an event that is expected for the Poll event
        $event = new Event(array(
            'request'    => $request,
            'curl_multi' => $multi
        ));
        $event->setName(CurlMultiInterface::POLLING_REQUEST);

        return $event;
    }
}
