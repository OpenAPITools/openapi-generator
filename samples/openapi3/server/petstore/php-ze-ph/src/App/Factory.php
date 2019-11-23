<?php
declare(strict_types=1);

namespace App;

use App\Middleware;
use Interop\Container\ContainerInterface;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Zend\Expressive\Application;
use Zend\Expressive\Handler\NotFoundHandler;
use Zend\Expressive\MiddlewareFactory;
use Zend\Expressive\Router\Middleware\DispatchMiddleware;
use Zend\Expressive\Router\Middleware\MethodNotAllowedMiddleware;
use Zend\Expressive\Router\Middleware\RouteMiddleware;
use Zend\Expressive\Router\RouteCollector;
use Zend\HttpHandlerRunner\Emitter\EmitterInterface;
use Zend\HttpHandlerRunner\RequestHandlerRunner;
use Zend\ServiceManager\Factory\FactoryInterface;
use Zend\Stratigility\MiddlewarePipe;

class Factory implements FactoryInterface
{
    public function __invoke(ContainerInterface $container, $requestedName, array $options = null): Application
    {
        $errorMiddleware = $container->get(Middleware\InternalServerError::class);
        if (!($errorMiddleware instanceof Middleware\InternalServerError)) {
            throw new \LogicException(\sprintf(
                'Invalid error middleware: expecting %s, not %s.',
                Middleware\InternalServerError::class,
                \is_object($errorMiddleware) ? \get_class($errorMiddleware) : \gettype($errorMiddleware)
            ));
        }
        $pipeline = new MiddlewarePipe();
        $runner = new RequestHandlerRunner(
            $pipeline,
            $container->get(EmitterInterface::class),
            $container->get(ServerRequestInterface::class),
            function(\Throwable $error) use ($errorMiddleware) : ResponseInterface
            {
                return $errorMiddleware->handleError($error);
            }
        );
        $application = new Application(
            $container->get(MiddlewareFactory::class),
            $pipeline,
            $container->get(RouteCollector::class),
            $runner
        );
        $application->pipe($errorMiddleware);
        $application->pipe(RouteMiddleware::class);
        $application->pipe(MethodNotAllowedMiddleware::class);
        $application->pipe(DispatchMiddleware::class);
        $application->pipe(NotFoundHandler::class);

        return $application;
    }
}
