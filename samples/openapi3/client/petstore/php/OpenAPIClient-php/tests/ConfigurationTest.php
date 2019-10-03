<?php

namespace OpenAPI\Client;

use GuzzleHttp\Client;
use PHPUnit\Framework\TestCase;

class ConfigurationTest extends TestCase
{
    /**
     * Test server settings
     */
    public function testMultipleServers()
    {
        $config = new Configuration();
        $servers = $config->getHostSettings();

        $this->assertCount(2, $servers);
        $this->assertSame("http://{server}.swagger.io:{port}/v2", $servers[0]["url"]);
        $this->assertSame("petstore", $servers[0]["variables"]["server"]["default_value"]);
        $this->assertSame("80", $servers[0]["variables"]["port"]["default_value"]);
        $this->assertSame(array("80", "8080"), $servers[0]["variables"]["port"]["enum_values"]);
    }

    /**
     * Test server settings
     */
    public function testServerUrl()
    {
        $config = new Configuration();
        // default value
        $url = $config->getHostFromSettings(0);
        $this->assertSame("http://petstore.swagger.io:80/v2", $url);

        // using a variable
        $url = $config->getHostFromSettings(0, array("server" => "dev-petstore"));
        $this->assertSame("http://dev-petstore.swagger.io:80/v2", $url);

        // using 2 variables
        $url = $config->getHostFromSettings(0, array("server" => "dev-petstore", "port" => "8080"));
        $this->assertSame("http://dev-petstore.swagger.io:8080/v2", $url);
    }

    /**
     * Test invalid index
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage Invalid index 2 when selecting the host. Must be less than 2
     */
    public function testInvalidIndex()
    {
        $config = new Configuration();
        $url = $config->getHostFromSettings(2);
    }

    /**
     * Test host settings with invalid vaues
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage The variable `port` in the host URL has invalid value 8. Must be 80,8080
     */
    public function testHostUrlWithInvalidValues()
    {
        // using 2 variables with invalid values
        $config = new Configuration();
        $url = $config->getHostFromSettings(0, array("server" => "dev-petstore", "port" => "8"));
        $this->assertSame("http://dev-petstore.swagger.io:8080/v2", $url);
    }
}
