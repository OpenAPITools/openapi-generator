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
        $data =  [
            // array $accept, string $contentType, bool $isMultipart, array $expectedHeaders
            [
                # No Accept, Content-Type
                [], 'application/xml', false, ['Content-Type' => 'application/xml'],
            ],
            [
                # No Accept, Content-Type, multipart
                [], 'application/xml', true, [],
            ],
            [
                # single Accept, no Content-Type
                ['application/xml'], '', false, ['Accept' => 'application/xml', 'Content-Type' => 'application/json'],
            ],
            [
                # single Accept, no Content-Type, multipart
                ['application/xml'], '', true, ['Accept' => 'application/xml'],
            ],
            [
                # single Accept, Content-Type
                ['application/xml'], 'application/xml', false, ['Accept' => 'application/xml', 'Content-Type' => 'application/xml'],
            ],
            [
                # single Accept, Content-Type, multipart
                ['application/xml'], 'application/xml', true, ['Accept' => 'application/xml'],
            ],
            [
                # single Accept with parameters, Content-Type with parameters
                ['application/json;format=flowed'], 'text/plain;format=fixed', false, ['Accept' => 'application/json;format=flowed', 'Content-Type' => 'text/plain;format=fixed'],
            ],

            # Tests for Accept headers - no change on Content-Type or multipart setting
            [
                # Multiple Accept without Json
                ['application/xml', 'text/html'], '', true, ['Accept' => 'application/xml,text/html'],
            ],
            [
                # Multiple Accept with application/json
                ['application/json', 'text/html'], '', true, ['Accept' => 'application/json,text/html;q=0.9'],
            ],
            [
                # Multiple Accept with json-like
                ['text/html', 'application/xml+json'], '', true, ['Accept' => 'application/xml+json,text/html;q=0.9'],
            ],
            [
                # Multiple Accept with application/json and json-like
                ['text/html', 'application/json', 'application/xml+json'], '', true, ['Accept' => 'application/json,application/xml+json;q=0.9,text/html;q=0.8'],
            ],
            [
                # Multiple Accept, changing priority to application/json
                ['application/xml', 'application/json;q=0.9', 'text/plain;format=flowed;q=0.8'], '', true, ['Accept' => 'application/json,application/xml;q=0.9,text/plain;format=flowed;q=0.8'],
            ],
            [
                # Multiple Accept, same priority for two headers, one being application/json
                ['application/xml', 'application/json', 'text/plain;format=flowed;q=0.9'], '', true, ['Accept' => 'application/json,application/xml;q=0.9,text/plain;format=flowed;q=0.8'],
            ],
            [
                # Multiple Accept, same priority for two headers, both being application/json
                ['application/json', 'application/json;IEEE754Compatible=true', 'text/plain;format=flowed;q=0.9'], '', true, ['Accept' => 'application/json,application/json;IEEE754Compatible=true,text/plain;format=flowed;q=0.9'],
            ],
            [
                # Multiple Accept, same priority for three headers, two being application/json, with changing priority
                ['application/json;q=0.9', 'application/json;IEEE754Compatible=true;q=0.9', 'application/xml', 'text/plain;format=flowed;q=0.9'], '', true, ['Accept' => 'application/json,application/json;IEEE754Compatible=true,application/xml;q=0.9,text/plain;format=flowed;q=0.8'],
            ],
        ];

        # More than 28 Accept headers
        $data[] = $this->createTestDataForLargeNumberOfAcceptHeaders(30);

        # More than 1000 Accept headers
        $data[] = $this->createTestDataForLargeNumberOfAcceptHeaders(1000);

        return $data;
    }

    /**
     * @param int $numberOfHeaders
     * @return array
     */
    private function createTestDataForLargeNumberOfAcceptHeaders(int $numberOfHeaders): array
    {
        $acceptHeaders = ['application/json;q=0.9'];
        $expected = ['application/json'];
        for ($i=1; $i<$numberOfHeaders; $i++) {
            $q = rtrim(sprintf('%0.3f', (1000 - $i) / 1000), '0');
            $acceptHeaders[] = "application/xml;option=$i;q=$q";
            $expected[] = "application/xml;option=$i;q=$q";
        }

        return [
            $acceptHeaders,
            '',
            true,
            ['Accept' => implode(',', $expected)]
        ];
    }

    /**
     * @dataProvider nextWeightProvider
     * @param int $currentWeight
     * @param bool $bHasMoreThan28Headers
     * @param int $expected
     */
    public function testGetNextWeight(int $currentWeight, bool $bHasMoreThan28Headers, int $expected): void
    {
        $selector = new HeaderSelector();

        self::assertEquals($expected, $selector->getNextWeight($currentWeight, $bHasMoreThan28Headers));
    }

    public function nextWeightProvider(): array
    {
        return [
            [1000, true, 999],
            [999, true, 998],
            [2, true, 1],
            [1, true, 1],
            [1000, false, 900],
            [900, false, 800],
            [200, false, 100],
            [100, false, 90],
            [90, false, 80],
            [20, false, 10],
            [10, false, 9],
            [9, false, 8],
            [2, false, 1],
            [1, false, 1],
            [0, false, 1],
        ];
    }
}
