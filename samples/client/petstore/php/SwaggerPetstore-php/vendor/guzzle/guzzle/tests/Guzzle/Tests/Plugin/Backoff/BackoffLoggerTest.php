<?php

namespace Guzzle\Tests\Plugin\Backoff;

use Guzzle\Common\Event;
use Guzzle\Log\ClosureLogAdapter;
use Guzzle\Http\Curl\CurlHandle;
use Guzzle\Plugin\Backoff\BackoffLogger;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Message\RequestFactory;

/**
 * @covers Guzzle\Plugin\Backoff\BackoffLogger
 */
class BackoffLoggerTest extends \Guzzle\Tests\GuzzleTestCase
{
    public $message;

    public function setUp()
    {
        $this->message = '';
    }

    public function testHasEventList()
    {
        $this->assertEquals(1, count(BackoffLogger::getSubscribedEvents()));
    }

    public function testLogsEvents()
    {
        list($logPlugin, $request, $response) = $this->getMocks();

        $response = $this->getMockBuilder('Guzzle\Http\Message\Response')
            ->setConstructorArgs(array(503))
            ->setMethods(array('getInfo'))
            ->getMock();

        $response->expects($this->any())
            ->method('getInfo')
            ->will($this->returnValue(2));

        $handle = $this->getMockHandle();

        $event = new Event(array(
            'request'  => $request,
            'response' => $response,
            'retries'  => 1,
            'delay'    => 3,
            'handle'   => $handle
        ));

        $logPlugin->onRequestRetry($event);
        $this->assertContains(
            '] PUT http://www.example.com - 503 Service Unavailable - Retries: 1, Delay: 3, Time: 2, 2, cURL: 30 Foo',
            $this->message
        );
    }

    public function testCanSetTemplate()
    {
        $l = new BackoffLogger(new ClosureLogAdapter(function () {}));
        $l->setTemplate('foo');
        $t = $this->readAttribute($l, 'formatter');
        $this->assertEquals('foo', $this->readAttribute($t, 'template'));
    }

    /**
     * @return array
     */
    protected function getMocks()
    {
        $that = $this;
        $logger = new ClosureLogAdapter(function ($message) use ($that) {
            $that->message .= $message . "\n";
        });
        $logPlugin = new BackoffLogger($logger);
        $response = new Response(503);
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.example.com', array(
            'Content-Length' => 3,
            'Foo'            => 'Bar'
        ));

        return array($logPlugin, $request, $response);
    }

    /**
     * @return CurlHandle
     */
    protected function getMockHandle()
    {
        $handle = $this->getMockBuilder('Guzzle\Http\Curl\CurlHandle')
            ->disableOriginalConstructor()
            ->setMethods(array('getError', 'getErrorNo', 'getInfo'))
            ->getMock();

        $handle->expects($this->once())
            ->method('getError')
            ->will($this->returnValue('Foo'));

        $handle->expects($this->once())
            ->method('getErrorNo')
            ->will($this->returnValue(30));

        $handle->expects($this->any())
            ->method('getInfo')
            ->will($this->returnValue(2));

        return $handle;
    }
}
