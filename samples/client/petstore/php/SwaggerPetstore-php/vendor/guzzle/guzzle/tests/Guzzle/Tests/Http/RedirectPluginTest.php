<?php

namespace Guzzle\Tests\Plugin\Redirect;

use Guzzle\Http\Client;
use Guzzle\Http\EntityBody;
use Guzzle\Http\RedirectPlugin;
use Guzzle\Http\Exception\TooManyRedirectsException;
use Guzzle\Plugin\History\HistoryPlugin;

/**
 * @covers Guzzle\Http\RedirectPlugin
 */
class RedirectPluginTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testRedirectsRequests()
    {
        // Flush the server and queue up a redirect followed by a successful response
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect1\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect2\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));

        // Create a client that uses the default redirect behavior
        $client = new Client($this->getServer()->getUrl());
        $history = new HistoryPlugin();
        $client->addSubscriber($history);

        $request = $client->get('/foo');
        $response = $request->send();
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertContains('/redirect2', $response->getEffectiveUrl());

        // Ensure that two requests were sent
        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('/foo', $requests[0]->getResource());
        $this->assertEquals('GET', $requests[0]->getMethod());
        $this->assertEquals('/redirect1', $requests[1]->getResource());
        $this->assertEquals('GET', $requests[1]->getMethod());
        $this->assertEquals('/redirect2', $requests[2]->getResource());
        $this->assertEquals('GET', $requests[2]->getMethod());

        // Ensure that the redirect count was incremented
        $this->assertEquals(2, $request->getParams()->get(RedirectPlugin::REDIRECT_COUNT));
        $this->assertCount(3, $history);
        $requestHistory = $history->getAll();

        $this->assertEquals(301, $requestHistory[0]['response']->getStatusCode());
        $this->assertEquals('/redirect1', (string) $requestHistory[0]['response']->getHeader('Location'));
        $this->assertEquals(301, $requestHistory[1]['response']->getStatusCode());
        $this->assertEquals('/redirect2', (string) $requestHistory[1]['response']->getHeader('Location'));
        $this->assertEquals(200, $requestHistory[2]['response']->getStatusCode());
    }

    public function testCanLimitNumberOfRedirects()
    {
        // Flush the server and queue up a redirect followed by a successful response
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect1\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect2\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect3\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect4\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect5\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect6\r\nContent-Length: 0\r\n\r\n"
        ));

        try {
            $client = new Client($this->getServer()->getUrl());
            $client->get('/foo')->send();
            $this->fail('Did not throw expected exception');
        } catch (TooManyRedirectsException $e) {
            $this->assertContains(
                "5 redirects were issued for this request:\nGET /foo HTTP/1.1\r\n",
                $e->getMessage()
            );
        }
    }

    public function testDefaultBehaviorIsToRedirectWithGetForEntityEnclosingRequests()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));

        $client = new Client($this->getServer()->getUrl());
        $client->post('/foo', array('X-Baz' => 'bar'), 'testing')->send();

        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('POST', $requests[0]->getMethod());
        $this->assertEquals('GET', $requests[1]->getMethod());
        $this->assertEquals('bar', (string) $requests[1]->getHeader('X-Baz'));
        $this->assertEquals('GET', $requests[2]->getMethod());
    }

    public function testCanRedirectWithStrictRfcCompliance()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));

        $client = new Client($this->getServer()->getUrl());
        $request = $client->post('/foo', array('X-Baz' => 'bar'), 'testing');
        $request->getParams()->set(RedirectPlugin::STRICT_REDIRECTS, true);
        $request->send();

        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('POST', $requests[0]->getMethod());
        $this->assertEquals('POST', $requests[1]->getMethod());
        $this->assertEquals('bar', (string) $requests[1]->getHeader('X-Baz'));
        $this->assertEquals('POST', $requests[2]->getMethod());
    }

    public function testRedirect303WithGet()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
             "HTTP/1.1 303 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
             "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));

        $client = new Client($this->getServer()->getUrl());
        $request = $client->post('/foo');
        $request->send();

        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('POST', $requests[0]->getMethod());
        $this->assertEquals('GET', $requests[1]->getMethod());
    }

    public function testRedirect303WithGetWithStrictRfcCompliance()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
             "HTTP/1.1 303 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
             "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));

        $client = new Client($this->getServer()->getUrl());
        $request = $client->post('/foo');
        $request->getParams()->set(RedirectPlugin::STRICT_REDIRECTS, true);
        $request->send();

        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('POST', $requests[0]->getMethod());
        $this->assertEquals('GET', $requests[1]->getMethod());
    }

    public function testRewindsStreamWhenRedirectingIfNeeded()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));

        $client = new Client($this->getServer()->getUrl());
        $request = $client->put();
        $request->configureRedirects(true);
        $body = EntityBody::factory('foo');
        $body->read(1);
        $request->setBody($body);
        $request->send();
        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('foo', (string) $requests[0]->getBody());
    }

    /**
     * @expectedException \Guzzle\Http\Exception\CouldNotRewindStreamException
     */
    public function testThrowsExceptionWhenStreamCannotBeRewound()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect\r\nContent-Length: 0\r\n\r\n"
        ));

        $client = new Client($this->getServer()->getUrl());
        $request = $client->put();
        $request->configureRedirects(true);
        $body = EntityBody::factory(fopen($this->getServer()->getUrl(), 'r'));
        $body->read(1);
        $request->setBody($body)->send();
    }

    public function testRedirectsCanBeDisabledPerRequest()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array("HTTP/1.1 301 Foo\r\nLocation: /foo\r\nContent-Length: 0\r\n\r\n"));
        $client = new Client($this->getServer()->getUrl());
        $request = $client->put();
        $request->configureRedirects(false, 0);
        $this->assertEquals(301, $request->send()->getStatusCode());
    }

    public function testCanRedirectWithNoLeadingSlashAndQuery()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: redirect?foo=bar\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
        ));
        $client = new Client($this->getServer()->getUrl());
        $request = $client->get('?foo=bar');
        $request->send();
        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals($this->getServer()->getUrl() . '?foo=bar', $requests[0]->getUrl());
        $this->assertEquals($this->getServer()->getUrl() . 'redirect?foo=bar', $requests[1]->getUrl());
        // Ensure that the history on the actual request is correct
        $this->assertEquals($this->getServer()->getUrl() . '?foo=bar', $request->getUrl());
    }

    public function testRedirectWithStrictRfc386Compliance()
    {
        // Flush the server and queue up a redirect followed by a successful response
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: redirect\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
        ));
        $client = new Client($this->getServer()->getUrl());
        $request = $client->get('/foo');
        $request->send();
        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('/redirect', $requests[1]->getResource());
    }

    public function testResetsHistoryEachSend()
    {
        // Flush the server and queue up a redirect followed by a successful response
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect1\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect2\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
        ));

        // Create a client that uses the default redirect behavior
        $client = new Client($this->getServer()->getUrl());
        $history = new HistoryPlugin();
        $client->addSubscriber($history);

        $request = $client->get('/foo');
        $response = $request->send();
        $this->assertEquals(3, count($history));
        $this->assertTrue($request->getParams()->hasKey('redirect.count'));
        $this->assertContains('/redirect2', $response->getEffectiveUrl());

        $request->send();
        $this->assertFalse($request->getParams()->hasKey('redirect.count'));
    }

    public function testHandlesRedirectsWithSpacesProperly()
    {
        // Flush the server and queue up a redirect followed by a successful response
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 301 Moved Permanently\r\nLocation: /redirect 1\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
        ));
        $client = new Client($this->getServer()->getUrl());
        $request = $client->get('/foo');
        $request->send();
        $reqs = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals('/redirect%201', $reqs[1]->getResource());
    }
}
