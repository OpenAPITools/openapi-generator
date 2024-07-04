<?php
declare(strict_types=1);

chdir(dirname(__DIR__));

/** @var \Laminas\ServiceManager\ServiceManager $container */
$container = require_once __DIR__.'/../application/container.php';

/** @var \Mezzio\Application $app */
$app = $container->get(\Mezzio\Application::class);
$app->run();
