<?php

namespace Guzzle\Tests\Http;

use Guzzle\Http\Exception\BadResponseException;
use Guzzle\Common\Exception\RuntimeException;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Message\RequestInterface;
use Guzzle\Http\Message\RequestFactory;
use Guzzle\Http\Client;

/**
 * The Server class is used to control a scripted webserver using node.js that
 * will respond to HTTP requests with queued responses.
 *
 * Queued responses will be served to requests using a FIFO order.  All requests
 * received by the server are stored on the node.js server and can be retrieved
 * by calling {@see Server::getReceivedRequests()}.
 *
 * Mock responses that don't require data to be transmitted over HTTP a great
 * for testing.  Mock response, however, cannot test the actual sending of an
 * HTTP request using cURL.  This test server allows the simulation of any
 * number of HTTP request response transactions to test the actual sending of
 * requests over the wire without having to leave an internal network.
 */
class Server
{
    const DEFAULT_PORT = 8124;
    const REQUEST_DELIMITER = "\n----[request]\n";

    /** @var int Port that the server will listen on */
    private $port;

    /** @var bool Is the server running */
    private $running = false;

    /** @var Client */
    private $client;

    /**
     * Create a new scripted server
     *
     * @param int $port Port to listen on (defaults to 8124)
     */
    public function __construct($port = null)
    {
        $this->port = $port ?: self::DEFAULT_PORT;
        $this->client = new Client($this->getUrl());
        register_shutdown_function(array($this, 'stop'));
    }

    /**
     * Flush the received requests from the server
     * @throws RuntimeException
     */
    public function flush()
    {
        $this->client->delete('guzzle-server/requests')->send();
    }

    /**
     * Queue an array of responses or a single response on the server.
     *
     * Any currently queued responses will be overwritten.  Subsequent requests
     * on the server will return queued responses in FIFO order.
     *
     * @param array|Response $responses A single or array of Responses to queue
     * @throws BadResponseException
     */
    public function enqueue($responses)
    {
        $data = array();
        foreach ((array) $responses as $response) {

            // Create the response object from a string
            if (is_string($response)) {
                $response = Response::fromMessage($response);
            } elseif (!($response instanceof Response)) {
                throw new BadResponseException('Responses must be strings or implement Response');
            }

            $data[] = array(
                'statusCode'   => $response->getStatusCode(),
                'reasonPhrase' => $response->getReasonPhrase(),
                'headers'      => $response->getHeaders()->toArray(),
                'body'         => $response->getBody(true)
            );
        }

        $request = $this->client->put('guzzle-server/responses', null, json_encode($data));
        $request->send();
    }

    /**
     * Check if the server is running
     *
     * @return bool
     */
    public function isRunning()
    {
        if ($this->running) {
            return true;
        }

        try {
            $this->client->get('guzzle-server/perf', array(), array('timeout' => 5))->send();
            $this->running = true;
            return true;
        } catch (\Exception $e) {
            return false;
        }
    }

    /**
     * Get the URL to the server
     *
     * @return string
     */
    public function getUrl()
    {
        return 'http://127.0.0.1:' . $this->getPort() . '/';
    }

    /**
     * Get the port that the server is listening on
     *
     * @return int
     */
    public function getPort()
    {
        return $this->port;
    }

    /**
     * Get all of the received requests
     *
     * @param bool $hydrate Set to TRUE to turn the messages into
     *      actual {@see RequestInterface} objects.  If $hydrate is FALSE,
     *      requests will be returned as strings.
     *
     * @return array
     * @throws RuntimeException
     */
    public function getReceivedRequests($hydrate = false)
    {
        $response = $this->client->get('guzzle-server/requests')->send();
        $data = array_filter(explode(self::REQUEST_DELIMITER, $response->getBody(true)));
        if ($hydrate) {
            $data = array_map(function($message) {
                return RequestFactory::getInstance()->fromMessage($message);
            }, $data);
        }

        return $data;
    }

    /**
     * Start running the node.js server in the background
     */
    public function start()
    {
        if (!$this->isRunning()) {
            exec('node ' . __DIR__ . \DIRECTORY_SEPARATOR
                . 'server.js ' . $this->port
                . ' >> /tmp/server.log 2>&1 &');
            // Wait at most 5 seconds for the server the setup before
            // proceeding.
            $start = time();
            while (!$this->isRunning() && time() - $start < 5);
            if (!$this->running) {
                throw new RuntimeException(
                    'Unable to contact server.js. Have you installed node.js v0.5.0+? node must be in your path.'
                );
            }
        }
    }

    /**
     * Stop running the node.js server
     */
    public function stop()
    {
        if (!$this->isRunning()) {
            return false;
        }

        $this->running = false;
        $this->client->delete('guzzle-server')->send();
    }
}
