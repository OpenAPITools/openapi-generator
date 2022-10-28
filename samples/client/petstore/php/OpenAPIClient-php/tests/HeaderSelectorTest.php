<?php
namespace OpenAPI\Client;

use PHPUnit\Framework\TestCase;

/**
 * class HeaderSelectorTest
 *
 * @package OpenAPI\Client
 */
class HeaderSelectorTest extends TestCase
{
    /**
     * @dataProvider headersProvider
     * @param string[] $accept
     * @param string   $contentType
     * @param bool     $isMultiple
     * @param string[] $expectedHeaders
     */
    public function testSelectHeaders(array $accept, string $contentType, bool $isMultiple, array $expectedHeaders): void
    {
        $selector = new HeaderSelector();

        $headers = $selector->selectHeaders($accept, $contentType, $isMultiple);
        $this->assertEquals($expectedHeaders, $headers);
    }

    /**
     * @return array[][]
     */
    public function headersProvider(): array
    {
        return [
            // array $accept, string $contentType, bool $isMultipart, array $expectedHeaders
            [
                [], 'application/xml', false, ['Content-Type' => 'application/xml'],
            ],
            [
                [], 'application/xml', true, [],
            ],
            [
                ['application/xml'], '', false, ['Accept' => 'application/xml', 'Content-Type' => 'application/json'],
            ],
            [
                ['application/xml'], '', true, ['Accept' => 'application/xml'],
            ],
            [
                ['application/xml'], 'application/xml', false, ['Accept' => 'application/xml', 'Content-Type' => 'application/xml'],
            ],
            [
                ['application/xml'], 'application/xml', true, ['Accept' => 'application/xml'],
            ],
            [
                ['application/xml', 'text/html'], 'application/xml', false, ['Accept' => 'application/xml,text/html', 'Content-Type' => 'application/xml'],
            ],
            [
                ['application/json', 'text/html'], 'application/xml', false, ['Accept' => 'application/json', 'Content-Type' => 'application/xml'],
            ],
            [
                ['text/html', 'application/json'], 'application/xml', false, ['Accept' => 'application/json', 'Content-Type' => 'application/xml'],
            ],
            [
                ['application/json;format=flowed'], 'text/plain;format=fixed', false, ['Accept' => 'application/json;format=flowed', 'Content-Type' => 'text/plain;format=fixed'],
            ],
            [
                ['text/html', 'application/json;format=flowed'], 'text/plain;format=fixed', false, ['Accept' => 'application/json;format=flowed', 'Content-Type' => 'text/plain;format=fixed'],
            ],
        ];
    }
}
