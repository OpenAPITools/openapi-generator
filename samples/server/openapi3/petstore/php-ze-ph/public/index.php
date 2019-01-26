<?php
declare(strict_types=1);

chdir(dirname(__DIR__));

/** @var \Zend\ServiceManager\ServiceManager $container */
$container = require_once __DIR__.'/../application/container.php';

/** @var \Zend\Expressive\Application $app */
$app = $container->get(\Zend\Expressive\Application::class);
$app->run();
