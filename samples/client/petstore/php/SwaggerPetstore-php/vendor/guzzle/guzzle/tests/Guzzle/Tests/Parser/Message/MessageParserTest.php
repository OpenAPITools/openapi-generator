<?php

namespace Guzzle\Tests\Parser\Message;

use Guzzle\Parser\Message\MessageParser;

/**
 * @covers Guzzle\Parser\Message\AbstractMessageParser
 * @covers Guzzle\Parser\Message\MessageParser
 */
class MessageParserTest extends MessageParserProvider
{
    /**
     * @dataProvider requestProvider
     */
    public function testParsesRequests($message, $parts)
    {
        $parser = new MessageParser();
        $this->compareRequestResults($parts, $parser->parseRequest($message));
    }

    /**
     * @dataProvider responseProvider
     */
    public function testParsesResponses($message, $parts)
    {
        $parser = new MessageParser();
        $this->compareResponseResults($parts, $parser->parseResponse($message));
    }

    public function testParsesRequestsWithMissingProtocol()
    {
        $parser = new MessageParser();
        $parts = $parser->parseRequest("GET /\r\nHost: Foo.com\r\n\r\n");
        $this->assertEquals('GET', $parts['method']);
        $this->assertEquals('HTTP', $parts['protocol']);
        $this->assertEquals('1.1', $parts['version']);
    }

    public function testParsesRequestsWithMissingVersion()
    {
        $parser = new MessageParser();
        $parts = $parser->parseRequest("GET / HTTP\r\nHost: Foo.com\r\n\r\n");
        $this->assertEquals('GET', $parts['method']);
        $this->assertEquals('HTTP', $parts['protocol']);
        $this->assertEquals('1.1', $parts['version']);
    }

    public function testParsesResponsesWithMissingReasonPhrase()
    {
        $parser = new MessageParser();
        $parts = $parser->parseResponse("HTTP/1.1 200\r\n\r\n");
        $this->assertEquals('200', $parts['code']);
        $this->assertEquals('', $parts['reason_phrase']);
        $this->assertEquals('HTTP', $parts['protocol']);
        $this->assertEquals('1.1', $parts['version']);
    }
}
