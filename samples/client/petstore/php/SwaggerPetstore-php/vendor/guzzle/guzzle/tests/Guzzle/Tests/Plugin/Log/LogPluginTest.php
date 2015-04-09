<?php

namespace Guzzle\Tests\Plugin\Log;

use Guzzle\Log\ClosureLogAdapter;
use Guzzle\Http\Client;
use Guzzle\Http\EntityBody;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Log\LogPlugin;
use Guzzle\Common\Event;

/**
 * @group server
 * @covers Guzzle\Plugin\Log\LogPlugin
 */
class LogPluginTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $adapter;

    public function setUp()
    {
        $this->adapter = new ClosureLogAdapter(function ($message) {
            echo $message;
        });
    }

    public function testIgnoresCurlEventsWhenNotWiringBodies()
    {
        $p = new LogPlugin($this->adapter);
        $this->assertNotEmpty($p->getSubscribedEvents());
        $event = new Event(array('request' => new Request('GET', 'http://foo.com')));
        $p->onCurlRead($event);
        $p->onCurlWrite($event);
        $p->onRequestBeforeSend($event);
    }

    public function testLogsWhenComplete()
    {
        $output = '';
        $p = new LogPlugin(new ClosureLogAdapter(function ($message) use (&$output) {
            $output = $message;
        }), '{method} {resource} | {code} {res_body}');

        $p->onRequestSent(new Event(array(
            'request'  => new Request('GET', 'http://foo.com'),
            'response' => new Response(200, array(), 'Foo')
        )));

        $this->assertEquals('GET / | 200 Foo', $output);
    }

    public function testWiresBodiesWhenNeeded()
    {
        $client = new Client($this->getServer()->getUrl());
        $plugin = new LogPlugin($this->adapter, '{req_body} | {res_body}', true);
        $client->getEventDispatcher()->addSubscriber($plugin);
        $request = $client->put();

        // Send the response from the dummy server as the request body
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nsend");
        $stream = fopen($this->getServer()->getUrl(), 'r');
        $request->setBody(EntityBody::factory($stream, 4));

        $tmpFile = tempnam(sys_get_temp_dir(), 'non_repeatable');
        $request->setResponseBody(EntityBody::factory(fopen($tmpFile, 'w')));

        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 8\r\n\r\nresponse");

        ob_start();
        $request->send();
        $message = ob_get_clean();

        unlink($tmpFile);
        $this->assertContains("send", $message);
        $this->assertContains("response", $message);
    }

    public function testHasHelpfulStaticFactoryMethod()
    {
        $s = fopen('php://temp', 'r+');
        $client = new Client();
        $client->addSubscriber(LogPlugin::getDebugPlugin(true, $s));
        $request = $client->put('http://foo.com', array('Content-Type' => 'Foo'), 'Bar');
        $request->setresponse(new Response(200), true);
        $request->send();
        rewind($s);
        $contents = stream_get_contents($s);
        $this->assertContains('# Request:', $contents);
        $this->assertContainsIns('PUT / HTTP/1.1', $contents);
        $this->assertContains('# Response:', $contents);
        $this->assertContainsIns('HTTP/1.1 200 OK', $contents);
        $this->assertContains('# Errors:', $contents);
    }
}
