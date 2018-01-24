<?php

namespace App;

use Interop\Http\ServerMiddleware\DelegateInterface;
use Interop\Http\ServerMiddleware\MiddlewareInterface;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Zend\Stdlib\ErrorHandler;

class ErrorMiddleware implements MiddlewareInterface
{
    public function process(Request $request, DelegateInterface $delegate)
    {
        $result = null;
        try {
            ErrorHandler::start();
            $result = $delegate->process($request);
            ErrorHandler::stop(true);
            if (!($result instanceof Response)) {
                throw new \RuntimeException(sprintf(
                    'Invalid response: expecting %s, got %s',
                    Response::class,
                    is_object($result)? get_class($result) : gettype($result)
                ));
            }
        }
        catch (\Exception $error) {
            $result = (new \Zend\Diactoros\Response())->withStatus(500, 'Internal server error');
            error_log((string)$error);
        }
        return $result;
    }
}
