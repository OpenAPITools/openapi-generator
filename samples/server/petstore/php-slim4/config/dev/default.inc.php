<?php

/**
 * App configuration defaults for development.
 * This file used when 'APP_ENV' variable set to 'dev' or 'development'. Check public/.htaccess file
 */

// Enable error reporting
error_reporting(E_ALL);
ini_set("display_errors", 1);

/**
 * Each environment(dev, prod) should contain two files default.inc.php and config.inc.php.
 * This is the first file with development defaults. It contains all data which can be safely committed
 * to VCS(version control system). For sensitive values(passwords, api keys, emails) use config.inc.php
 * and make sure it's excluded from VCS by .gitignore.
 * do not add dependencies here, use OpenAPIServer\App\RegisterDependencies class
 * @see https://php-di.org/doc/php-definitions.html#values
 */
return [
    'mode' => 'development',

    // Returns a detailed HTML page with error details and
    // a stack trace. Should be disabled in production.
    'slim.displayErrorDetails' => true,

    // Whether to display errors on the internal PHP log or not.
    'slim.logErrors' => false,

    // If true, display full errors with message and stack trace on the PHP log.
    // If false, display only "Slim Application Error" on the PHP log.
    // Doesn't do anything when 'logErrors' is false.
    'slim.logErrorDetails' => false,

    // CORS settings
    // @see https://github.com/neomerx/cors-psr7/blob/master/src/Strategies/Settings.php
    'cors.settings' => [
        isset($_SERVER['HTTPS']) ? 'https' : 'http', // serverOriginScheme
        $_SERVER['SERVER_NAME'], // serverOriginHost
        null, // serverOriginPort
        false, // isPreFlightCanBeCached
        0, // preFlightCacheMaxAge
        false, // isForceAddMethods
        false, // isForceAddHeaders
        true, // isUseCredentials
        true, // areAllOriginsAllowed
        [], // allowedOrigins
        true, // areAllMethodsAllowed
        [], // allowedLcMethods
        'GET, POST, PUT, PATCH, DELETE', // allowedMethodsList
        true, // areAllHeadersAllowed
        [], // allowedLcHeaders
        'authorization, content-type, x-requested-with', // allowedHeadersList
        '', // exposedHeadersList
        true, // isCheckHost
    ],

    // PDO
    'pdo.dsn' => 'mysql:host=localhost;charset=utf8mb4',
    'pdo.username' => 'root',
    'pdo.password' => 'root',
    'pdo.options' => [
        \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
    ],

    // mocker
    // OBVIOUSLY MUST NOT BE USED for production
    // @see https://github.com/ybelenko/openapi-data-mocker-server-middleware
    'mocker.getMockStatusCodeCallback' => function () {
        return function (\Psr\Http\Message\ServerRequestInterface $request, array $responses) {
            // check if client clearly asks for mocked response
            $pingHeader = 'X-OpenAPIServer-Mock';
            $pingHeaderCode = 'X-OpenAPIServer-Mock-Code';
            if (
                $request->hasHeader($pingHeader)
                && $request->getHeader($pingHeader)[0] === 'ping'
            ) {
                $responses = (array) $responses;
                $requestedResponseCode = ($request->hasHeader($pingHeaderCode)) ? $request->getHeader($pingHeaderCode)[0] : 'default';
                if (array_key_exists($requestedResponseCode, $responses)) {
                    return $requestedResponseCode;
                }

                // return first response key
                reset($responses);
                return key($responses);
            }

            return false;
        };
    },
    'mocker.afterCallback' => function () {
        return function (\Psr\Http\Message\ServerRequestInterface $request, \Psr\Http\Message\ResponseInterface $response) {
            // mark mocked response to distinguish real and fake responses
            return $response->withHeader('X-OpenAPIServer-Mock', 'pong');
        };
    },

    // logger
    'logger.name' => 'App',
    'logger.path' => \realpath(__DIR__ . '/../../logs') . '/app.log',
    'logger.level' => 100, // equals DEBUG level
    'logger.options' => [],
];
