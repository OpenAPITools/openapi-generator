<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Api;

use Guzzle\Http\Client;
use Contrib\Bundle\CoverallsV1Bundle\Config\Configuration;

/**
 * Coveralls API client.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
abstract class CoverallsApi
{
    /**
     * Configuration.
     *
     * @var Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    protected $config;

    /**
     * HTTP client.
     *
     * @var \Guzzle\Http\Client
     */
    protected $client;

    /**
     * Constructor.
     *
     * @param Configuration       $config Configuration.
     * @param \Guzzle\Http\Client $client HTTP client.
     */
    public function __construct(Configuration $config, Client $client = null)
    {
        $this->config = $config;
        $this->client = $client;
    }

    // accessor

    /**
     * Return configuration.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function getConfiguration()
    {
        return $this->config;
    }

    /**
     * Set HTTP client.
     *
     * @param  \Guzzle\Http\Client                                $client HTTP client.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Api\CoverallsApi
     */
    public function setHttpClient(Client $client)
    {
        $this->client = $client;

        return $this;
    }

    /**
     * Return HTTP client.
     *
     * @return \Guzzle\Http\Client
     */
    public function getHttpClient()
    {
        return $this->client;
    }
}
