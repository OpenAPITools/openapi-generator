<?php

namespace OpenAPI\Client;

use GuzzleHttp\ClientInterface;
use GuzzleHttp\Exception\GuzzleException;
use GuzzleHttp\Psr7\Response;
use Psr\Http\Message\RequestInterface;
use Psr\Http\Message\ResponseInterface;

class FakeHttpClient implements ClientInterface
{
    /** @var  RequestInterface|null */
    private $request;
    /** @var  ResponseInterface|null */
    private $response;

    /**
     * @return null|RequestInterface
     */
    public function getLastRequest()
    {
        return $this->request;
    }

    /**
     * @param null|ResponseInterface $response
     */
    public function setResponse(ResponseInterface $response = null)
    {
        $this->response = $response;
    }

    /**
     * Send an HTTP request.
     *
     * @param RequestInterface $request Request to send
     * @param array $options Request options to apply to the given
     *                                  request and to the transfer.
     *
     * @return ResponseInterface
     * @throws GuzzleException
     */
    public function send(RequestInterface $request, array $options = [])
    {
        $this->request = $request;
        return $this->response ?: new Response(200);
    }

    public function sendAsync(RequestInterface $request, array $options = [])
    {
        throw new \RuntimeException('not implemented');
    }

    public function request($method, $uri, array $options = [])
    {
        throw new \RuntimeException('not implemented');
    }

    public function requestAsync($method, $uri, array $options = [])
    {
        throw new \RuntimeException('not implemented');
    }

    public function getConfig($option = null)
    {
        throw new \RuntimeException('not implemented');
    }
}
