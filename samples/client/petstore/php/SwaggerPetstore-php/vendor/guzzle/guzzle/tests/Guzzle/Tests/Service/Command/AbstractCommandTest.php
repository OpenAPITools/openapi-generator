<?php

namespace Guzzle\Tests\Service\Command;

use Guzzle\Service\Client;
use Guzzle\Service\Description\ServiceDescription;

abstract class AbstractCommandTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected function getClient()
    {
        $client = new Client('http://www.google.com/');

        return $client->setDescription(ServiceDescription::factory(__DIR__ . '/../../TestData/test_service.json'));
    }
}
