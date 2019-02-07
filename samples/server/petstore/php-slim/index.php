<?php
/**
 * OpenAPI Petstore
 *
 * PHP version 7
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
 * Slim Framework Settings
 * Ref: http://www.slimframework.com/docs/v3/objects/application.html#slim-default-settings
 */
$config['settings'] = [
    /**
     * The protocol version used by the Response object. (Default: '1.1')
     * Default: '1.1'
     */
    // 'httpVersion' => '1.1',

    /**
     * Size of each chunk read from the Response body when sending to the browser.
     * Default: 4096
     */
    // 'responseChunkSize' => 4096,

    /**
     * If false, then no output buffering is enabled. If 'append' or 'prepend', then
     * any echo or print statements are captured and are either appended or prepended
     * to the Response returned from the route callable.
     * Default: 'append'
     */
    // 'outputBuffering' => 'append',

    /**
     * When true, the route is calculated before any middleware is executed. This
     * means that you can inspect route parameters in middleware if you need to.
     * Default: false
     */
    // 'determineRouteBeforeAppMiddleware' => false,

    /**
     * When true, additional information about exceptions are displayed by the default
     * error handler.
     * Default: false
     */
    // 'displayErrorDetails' => false,

    /**
     * When true, Slim will add a Content-Length header to the response. If you are using
     * a runtime analytics tool, such as New Relic, then this should be disabled.
     * Default: true
     */
    // 'addContentLengthHeader' => true,

    /**
     * Filename for caching the FastRoute routes. Must be set to to a valid filename within
     * a writeable directory. If the file does not exist, then it is created with the correct
     * cache information on first run.
     * Set to false to disable the FastRoute cache system.
     * Default: false
     */
    // 'routerCacheFile' => false,
];

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
$app->run();
