<?php

namespace Guzzle\Tests\Log;

use Guzzle\Http\Client;
use Guzzle\Http\Curl\CurlHandle;
use Guzzle\Http\Message\EntityEnclosingRequest;
use Guzzle\Http\EntityBody;
use Guzzle\Http\Message\Response;
use Guzzle\Log\MessageFormatter;
use Guzzle\Plugin\Log\LogPlugin;
use Guzzle\Log\ClosureLogAdapter;

/**
 * @covers Guzzle\Log\MessageFormatter
 */
class MessageFormatterTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $request;
    protected $response;
    protected $handle;

    public function setUp()
    {
        $this->request = new EntityEnclosingRequest('POST', 'http://foo.com?q=test', array(
            'X-Foo'         => 'bar',
            'Authorization' => 'Baz'
        ));
        $this->request->setBody(EntityBody::factory('Hello'));

        $this->response = new Response(200, array(
            'X-Test' => 'Abc'
        ), 'Foo');

        $this->handle = $this->getMockBuilder('Guzzle\Http\Curl\CurlHandle')
            ->disableOriginalConstructor()
            ->setMethods(array('getError', 'getErrorNo', 'getStderr', 'getInfo'))
            ->getMock();

        $this->handle->expects($this->any())
            ->method('getError')
            ->will($this->returnValue('e'));

        $this->handle->expects($this->any())
            ->method('getErrorNo')
            ->will($this->returnValue('123'));

        $this->handle->expects($this->any())
            ->method('getStderr')
            ->will($this->returnValue('testing'));

        $this->handle->expects($this->any())
            ->method('getInfo')
            ->will($this->returnValueMap(array(
                array(CURLINFO_CONNECT_TIME, '123'),
                array(CURLINFO_TOTAL_TIME, '456')
            )));
    }

    public function logProvider()
    {
        return array(
            // Uses the cache for the second time
            array('{method} - {method}', 'POST - POST'),
            array('{url}', 'http://foo.com?q=test'),
            array('{port}', '80'),
            array('{resource}', '/?q=test'),
            array('{host}', 'foo.com'),
            array('{hostname}', gethostname()),
            array('{protocol}/{version}', 'HTTP/1.1'),
            array('{code} {phrase}', '200 OK'),
            array('{req_header_Foo}', ''),
            array('{req_header_X-Foo}', 'bar'),
            array('{req_header_Authorization}', 'Baz'),
            array('{res_header_foo}', ''),
            array('{res_header_X-Test}', 'Abc'),
            array('{req_body}', 'Hello'),
            array('{res_body}', 'Foo'),
            array('{curl_stderr}', 'testing'),
            array('{curl_error}', 'e'),
            array('{curl_code}', '123'),
            array('{connect_time}', '123'),
            array('{total_time}', '456')
        );
    }

    /**
     * @dataProvider logProvider
     */
    public function testFormatsMessages($template, $output)
    {
        $formatter = new MessageFormatter($template);
        $this->assertEquals($output, $formatter->format($this->request, $this->response, $this->handle));
    }

    public function testFormatsRequestsAndResponses()
    {
        $formatter = new MessageFormatter();
        $formatter->setTemplate('{request}{response}');
        $this->assertEquals($this->request . $this->response, $formatter->format($this->request, $this->response));
    }

    public function testAddsTimestamp()
    {
        $formatter = new MessageFormatter('{ts}');
        $this->assertNotEmpty($formatter->format($this->request, $this->response));
    }

    public function testUsesResponseWhenNoHandleAndGettingCurlInformation()
    {
        $formatter = new MessageFormatter('{connect_time}/{total_time}');
        $response = $this->getMockBuilder('Guzzle\Http\Message\Response')
            ->setConstructorArgs(array(200))
            ->setMethods(array('getInfo'))
            ->getMock();
        $response->expects($this->exactly(2))
            ->method('getInfo')
            ->will($this->returnValueMap(array(
                array('connect_time', '1'),
                array('total_time', '2'),
            )));
        $this->assertEquals('1/2', $formatter->format($this->request, $response));
    }

    public function testUsesEmptyStringWhenNoHandleAndNoResponse()
    {
        $formatter = new MessageFormatter('{connect_time}/{total_time}');
        $this->assertEquals('/', $formatter->format($this->request));
    }

    public function testInjectsTotalTime()
    {
        $out = '';
        $formatter = new MessageFormatter('{connect_time}/{total_time}');
        $adapter = new ClosureLogAdapter(function ($m) use (&$out) { $out .= $m; });
        $log = new LogPlugin($adapter, $formatter);
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nHI");
        $client = new Client($this->getServer()->getUrl());
        $client->addSubscriber($log);
        $client->get('/')->send();
        $this->assertNotEquals('/', $out);
    }
}
