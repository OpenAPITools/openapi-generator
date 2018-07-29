<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Silex\Application;

$app = new Silex\Application();


$app->PUT('/fake', function(Application $app, Request $request) {
            return new Response('How about implementing testCodeInject */ &#39; &quot; &#x3D;end  \r\n \n \r as a PUT method ?');
            });


$app->run();
