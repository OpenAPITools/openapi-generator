<?php
/**
 * OpenAPI Petstore  &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 * @version 1.0.0  &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 */

require_once __DIR__ . '/vendor/autoload.php';

$app = new Slim\App();


/**
 * PUT testCodeInjectEndRnNR
 * Summary: To test code injection  &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 * Notes: 
 */
$app->PUT('/fake', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing testCodeInjectEndRnNR as a PUT method ?');
    return $response;
});



$app->run();
