<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Silex\Application;

$app = new Silex\Application();


$app->PUT('/v2 *_/ ' \" =end -- \\r\\n \\n \\r/fake', function(Application $app, Request $request) {
            $test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r = $request->get('test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r');
            return new Response('How about implementing testCodeInject */ &#39; &quot; &#x3D;end  \r\n \n \r as a PUT method ?');
            });


$app->run();
