<?php
/**
 * OpenAPI Petstore
 *
 * PHP version 7.1
 *
 * @package OpenAPIServer\Api
 * @author  OpenAPI Generator team
 * @version 1.0.0
 * @link    https://github.com/openapitools/openapi-generator
 */

require_once __DIR__ . '/vendor/autoload.php';

use OpenAPIServer\SlimRouter;

$config = [];

/**
 * Token Middleware 1.x Options
 * Options `header`, `regex`, `parameter`, `cookie`, `attribute`, `path`, `except`, `authenticator`
 * are handled by SlimRouter class. These options are ignored by app and they omitted from current
 * example.
 * Ref: https://github.com/dyorg/slim-token-authentication/tree/1.x
 */
$config['tokenAuthenticationOptions'] = [
    /**
     * Tokens are essentially passwords. You should treat them as such and you should always
     * use HTTPS. If the middleware detects insecure usage over HTTP it will return unathorized
     * with a message Required HTTPS for token authentication. This rule is relaxed for requests
     * on localhost. To allow insecure usage you must enable it manually by setting secure to
     * false.
     * Default: true
     */
    // 'secure' => true,

    /**
     * Alternatively you can list your development host to have relaxed security.
     * Default: ['localhost', '127.0.0.1']
     */
    // 'relaxed' => ['localhost', '127.0.0.1'],

    /**
     * By default on ocurred a fail on authentication, is sent a response on json format with a
     * message (`Invalid Token` or `Not found Token`) and with the token (if found), with status
     * `401 Unauthorized`. You can customize it by setting a callable function on error option.
     * Default: null
     */
    // 'error' => null,
];

$router = new SlimRouter($config);
$app = $router->getSlimApp();

/**
 * The routing middleware should be added before the ErrorMiddleware
 * Otherwise exceptions thrown from it will not be handled
 */
$app->addRoutingMiddleware();

/**
 * Add Error Handling Middleware
 *
 * @param bool $displayErrorDetails -> Should be set to false in production
 * @param bool $logErrors -> Parameter is passed to the default ErrorHandler
 * @param bool $logErrorDetails -> Display error details in error log
 * which can be replaced by a callable of your choice.

 * Note: This middleware should be added last. It will not handle any exceptions/errors
 * for middleware added after it.
 */
$app->addErrorMiddleware(false, true, true);

$app->run();
