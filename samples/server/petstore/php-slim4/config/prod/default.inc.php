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

    // slim framework settings
    'slim.displayErrorDetails' => false,
    'slim.logErrors' => true,
    'slim.logErrorDetails' => true,

    // PDO
    'pdo.dsn' => 'mysql:host=localhost;charset=utf8mb4',
    'pdo.username' => 'root',
    'pdo.password' => 'root',
    'pdo.options' => [
        \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
    ],
];
