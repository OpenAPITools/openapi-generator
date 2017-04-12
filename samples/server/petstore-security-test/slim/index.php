<?php
/**
 * Swagger Petstore  &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 * @version 1.0.0  &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 */

require_once __DIR__ . '/vendor/autoload.php';

$app = new Slim\App();


/**
 * PUT testCodeInject */ &#39; &quot; &#x3D;end  \r\n \n \r
 * Summary: To test code injection  &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 * Notes: 
 * Output-Formats: [application/json,   \" =end --       ]
 */
$app->PUT('/v2  ' \" =end -- \\r\\n \\n \\r/fake', function($request, $response, $args) {
            
            
            $testCodeInjectEndRnNR = $args['testCodeInjectEndRnNR'];    
            
            $response->write('How about implementing testCodeInject */ &#39; &quot; &#x3D;end  \r\n \n \r as a PUT method ?');
            return $response;
            });



$app->run();
