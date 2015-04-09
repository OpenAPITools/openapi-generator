<?php

namespace Guzzle\Tests\Service\Mock;

use Guzzle\Common\Collection;
use Guzzle\Service\Client;

/**
 * Mock Guzzle Service
 */
class MockClient extends Client
{
    /**
     * Factory method to create a new mock client
     *
     * @param array|Collection $config Configuration data. Array keys:
     *    base_url - Base URL of web service
     *    api_version - API version
     *    scheme - URI scheme: http or https
     *  * username - API username
     *  * password - API password
     *  * subdomain - Unfuddle account subdomain
     *
     * @return MockClient
     */
    public static function factory($config = array())
    {
        $config = Collection::fromConfig($config, array(
            'base_url' => '{scheme}://127.0.0.1:8124/{api_version}/{subdomain}',
            'scheme' => 'http',
            'api_version' => 'v1'
        ), array('username', 'password', 'subdomain'));

        return new self($config->get('base_url'), $config);
    }
}
