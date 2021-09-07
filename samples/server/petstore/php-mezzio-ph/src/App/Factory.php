<?php
declare(strict_types=1);

namespace App;

use App\Middleware;
use Interop\Container\ContainerInterface;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Mezzio\Application;
use Mezzio\Handler\NotFoundHandler;
use Mezzio\MiddlewareFactory;
use Mezzio\Router\Middleware\DispatchMiddleware;
use Mezzio\Router\Middleware\MethodNotAllowedMiddleware;
use Mezzio\Router\Middleware\RouteMiddleware;
use Mezzio\Router\RouteCollector;
use Laminas\HttpHandlerRunner\Emitter\EmitterInterface;
use Laminas\HttpHandlerRunner\RequestHandlerRunner;
use Laminas\ServiceManager\Factory\FactoryInterface;
use Laminas\Stratigility\MiddlewarePipe;

class Factory implements FactoryInterface
{
    public function __invoke(ContainerInterface $container, $requestedName, array $options = null): Application
    {
        $errorMiddleware = self::getErrorMiddleware($container);
        $pipeline = new MiddlewarePipe();
        $runner = new RequestHandlerRunner(
            $pipeline,
            $container->get(EmitterInterface::class),
            $container->get(ServerRequestInterface::class),
            static function(\Throwable $error) use ($errorMiddleware) : ResponseInterface
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

    protected static function getErrorMiddleware(ContainerInterface $container): Middleware\InternalServerError
    {
        return $container->get(Middleware\InternalServerError::class);
    }
}
