<?php

/**
 * App configuration defaults for production.
 * This file used when 'APP_ENV' variable set to 'prod' or 'production'. Check public/.htaccess file
 */

// Disable error reporting
error_reporting(0);
ini_set('display_errors', '0');

/**
 * Each environment(dev, prod) should contain two files default.inc.php and config.inc.php.
 * This is the first file with production defaults. It contains all data which can be safely committed
 * to VCS(version control system). For sensitive values(passwords, api keys, emails) use config.inc.php
 * and make sure it's excluded from VCS by .gitignore.
 * do not add dependencies here, use OpenAPIServer\App\RegisterDependencies class
 * @see https://php-di.org/doc/php-definitions.html#values
 */
return [
    'mode' => 'production',

    // Returns a detailed HTML page with error details and
    // a stack trace. Should be disabled in production.
    'slim.displayErrorDetails' => false,

    // Whether to display errors on the internal PHP log or not.
    'slim.logErrors' => true,

    // If true, display full errors with message and stack trace on the PHP log.
    // If false, display only "Slim Application Error" on the PHP log.
    // Doesn't do anything when 'logErrors' is false.
    'slim.logErrorDetails' => true,

    // CORS settings
    // https://github.com/neomerx/cors-psr7/blob/master/src/Strategies/Settings.php
    'cors.settings' => [
        isset($_SERVER['HTTPS']) ? 'https' : 'http', // serverOriginScheme
        $_SERVER['SERVER_NAME'], // serverOriginHost
        null, // serverOriginPort
        true, // isPreFlightCanBeCached
        86400, // preFlightCacheMaxAge
        false, // isForceAddMethods
        false, // isForceAddHeaders
        true, // isUseCredentials
        false, // areAllOriginsAllowed
        [], // allowedOrigins
        false, // areAllMethodsAllowed
        [], // allowedLcMethods
        '', // allowedMethodsList
        false, // areAllHeadersAllowed
        [], // allowedLcHeaders
        '', // allowedHeadersList
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

    // logger
    'logger.name' => 'App',
    'logger.path' => \realpath(__DIR__ . '/../../logs') . '/app.log',
    'logger.level' => 300, // equals WARNING level
    'logger.options' => [],
];
