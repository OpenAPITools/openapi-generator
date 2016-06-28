<?php
/**
 * Swagger Petstore  &#39; \&quot; &#x3D;end
 * @version 1.0.0  &#39; \&quot; &#x3D;end
 */

require_once __DIR__ . '/vendor/autoload.php';

$app = new Slim\App();


/**
 * PUT testCodeInject */ &#39; &quot; &#x3D;end
 * Summary: To test code injection  &#39; \&quot; &#x3D;end
 * Notes: 
 * Output-Formats: [application/json, */  " =end]
 */
$app->PUT('/fake', function($request, $response, $args) {
            
            
            $testCodeInjectEnd = $args['testCodeInjectEnd'];    
            
            $response->write('How about implementing testCodeInject */ &#39; &quot; &#x3D;end as a PUT method ?');
            return $response;
            });



$app->run();
