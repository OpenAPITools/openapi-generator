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

    // slim framework settings
    'slim.displayErrorDetails' => true,
    'slim.logErrors' => false,
    'slim.logErrorDetails' => false,

    // PDO
    'pdo.dsn' => 'mysql:host=localhost;charset=utf8mb4',
    'pdo.username' => 'root',
    'pdo.password' => 'root',
    'pdo.options' => [
        \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
    ],
];
