<?php
namespace App;

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Zend\Stratigility\ErrorMiddlewareInterface;

class ErrorMiddleware implements ErrorMiddlewareInterface
{
    /**
     * @inheritDoc
     */
    public function __invoke($error, Request $request, Response $response, callable $out = null)
    {
        $response = $response->withStatus(500, 'Internal server error');
        $response->getBody()->write((string)$error);
        error_log((string) $error);
        return ($out === null)? $response : $out($request, $response);
    }
}