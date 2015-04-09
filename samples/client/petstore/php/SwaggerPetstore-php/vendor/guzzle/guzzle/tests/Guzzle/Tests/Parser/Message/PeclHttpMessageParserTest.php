<?php

namespace Guzzle\Tests\Parser\Message;

use Guzzle\Parser\Message\PeclHttpMessageParser;

/**
 * @covers Guzzle\Parser\Message\PeclHttpMessageParser
 */
class PeclHttpMessageParserTest extends MessageParserProvider
{
    protected function setUp()
    {
        if (!function_exists('http_parse_message')) {
            $this->markTestSkipped('pecl_http is not available.');
        }
    }

    /**
     * @dataProvider requestProvider
     */
    public function testParsesRequests($message, $parts)
    {
        $parser = new PeclHttpMessageParser();
        $this->compareRequestResults($parts, $parser->parseRequest($message));
    }

    /**
     * @dataProvider responseProvider
     */
    public function testParsesResponses($message, $parts)
    {
        $parser = new PeclHttpMessageParser();
        $this->compareResponseResults($parts, $parser->parseResponse($message));
    }
}
