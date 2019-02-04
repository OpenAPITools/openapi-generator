<?php
declare(strict_types=1);

namespace App\Middleware;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\MiddlewareInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Zend\Stdlib\ErrorHandler;

class InternalServerError implements MiddlewareInterface
{
    /**
     * @var callable
     */
    protected $responseGenerator;

    /**
     * @param callable $responseGenerator
     */
    public function __construct(callable $responseGenerator)
    {
        $this->responseGenerator = $responseGenerator;
    }

    /**
     * @inheritdoc
     */
    public function process(ServerRequestInterface $request, RequestHandlerInterface $handler): ResponseInterface
    {
        $result = null;
        try {
            ErrorHandler::start();
            $result = $handler->handle($request);
            ErrorHandler::stop(true);
        }
        catch (\Throwable $error) {
            $result = $this->handleError($error);
        }
        return $result;
    }

    public function handleError(\Throwable $error): ResponseInterface
    {
        \error_log((string)$error);
        return $this->generateEmptyResponse()->withStatus(500, 'Internal server error');
    }

    protected function generateEmptyResponse(): ResponseInterface
    {
        return ($this->responseGenerator)();
    }
}
