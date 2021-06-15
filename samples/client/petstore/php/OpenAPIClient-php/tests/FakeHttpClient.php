<?php

namespace OpenAPI\Client;

use Psr\Http\Client\ClientInterface;
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

    public function sendRequest(RequestInterface $request): ResponseInterface
    {
        $this->request = $request;
        return $this->response ?: new \GuzzleHttp\Psr7\Response(200);
    }
}
