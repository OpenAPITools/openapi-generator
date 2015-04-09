<?php
error_reporting(E_ALL | E_STRICT);

require_once 'PHPUnit/TextUI/TestRunner.php';
require dirname(__DIR__) . '/vendor/autoload.php';

// Add the services file to the default service builder
$servicesFile = __DIR__ . '/Guzzle/Tests/TestData/services/services.json';
$builder = Guzzle\Service\Builder\ServiceBuilder::factory($servicesFile);
Guzzle\Tests\GuzzleTestCase::setServiceBuilder($builder);
