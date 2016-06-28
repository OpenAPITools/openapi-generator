<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Silex\Application;

$app = new Silex\Application();


$app->PUT('/fake', function(Application $app, Request $request) {
            
            $test code inject */ &#39; &quot; &#x3D;end = $request->get('test code inject */ &#39; &quot; &#x3D;end');    
            return new Response('How about implementing testCodeInject */ &#39; &quot; &#x3D;end as a PUT method ?');
            });


$app->run();
