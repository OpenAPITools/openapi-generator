<?php

namespace OpenAPI\Client;

use GuzzleHttp\Psr7\Utils;
use PHPUnit\Framework\TestCase;

// test object serializer
class ObjectSerializerTest extends TestCase
{
    // test sanitizeFilename
    public function testSanitizeFilename()
    {
        // initialize the API client
        $s = new ObjectSerializer();

        $this->assertSame("sun.gif", $s->sanitizeFilename("sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("../sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("/var/tmp/sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("./sun.gif"));
        
        $this->assertSame("sun", $s->sanitizeFilename("sun"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("..\sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("\var\tmp\sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename("c:\var\tmp\sun.gif"));
        $this->assertSame("sun.gif", $s->sanitizeFilename(".\sun.gif"));
    }

    /**
     * Test SplFileObject class deserialization.
     *
     * @see https://github.com/OpenAPITools/openapi-generator/pull/11184
     * @covers ObjectSerializer::serialize
     * @dataProvider provideFileStreams
     */
    public function testDeserializeFile($stream, ?array $httpHeaders = null, ?string $expectedFilename = null): void
    {
        $s = new ObjectSerializer();

        /** @var \SplFileObject */
        $file = $s->deserialize($stream, '\SplFileObject', $httpHeaders);
        $this->assertInstanceOf(\SplFileObject::class, $file);

        if (is_string($expectedFilename)) {
            $this->assertEquals($expectedFilename, $file->getFilename());
        } else {
            $this->assertNotEquals($expectedFilename, $file->getFilename());
        }
    }

    public function provideFileStreams()
    {
        return [
            'File stream without headers' => [
                Utils::streamFor(\fopen(__FILE__, 'r')),
                null,
                null,
            ],
            'File stream with Content-Disposition header' => [
                Utils::streamFor(\fopen(__FILE__, 'r')),
                ['Content-Disposition' => 'inline; filename=\'foobar.php\''],
                'foobar.php',
            ],
            'File path' => [
                __FILE__,
                null,
                null,
            ],
        ];
    }

    /**
     * @covers ObjectSerializer::sanitizeTimestamp
     * @dataProvider provideTimestamps
     */
    public function testSanitizeTimestamp(string $timestamp, string $expected): void
    {
        $this->assertEquals($expected, ObjectSerializer::sanitizeTimestamp($timestamp));
    }

    /**
     * Test datetime deserialization.
     *
     * @covers ObjectSerializer::deserialize
     * @dataProvider provideTimestamps
     *
     * @see https://github.com/OpenAPITools/openapi-generator/issues/7942
     * @see https://github.com/OpenAPITools/openapi-generator/issues/10548
     */
    public function testDateTimeParseSecondAccuracy(string $timestamp, string $expected): void
    {
        $dateTime = ObjectSerializer::deserialize($timestamp, '\DateTime');
        $this->assertEquals(new \DateTime($expected), $dateTime);
    }

    public function provideTimestamps(): array
    {
        return [
            'String from #7942' => [
                '2020-11-11T15:17:58.868722633Z',
                '2020-11-11T15:17:58.868722Z',
            ],
            'String from #10548' => [
                '2021-10-06T20:17:16.076372256Z',
                '2021-10-06T20:17:16.076372Z',
            ],
            'Without timezone' => [
                '2021-10-06T20:17:16.076372256',
                '2021-10-06T20:17:16.076372',
            ],
            'Without microseconds' => [
                '2021-10-06T20:17:16',
                '2021-10-06T20:17:16',
            ],
            'Without microseconds with timezone' => [
                '2021-10-06T20:17:16Z',
                '2021-10-06T20:17:16Z',
            ],
            'With numeric timezone' => [
                '2021-10-06T20:17:16.076372256+03:00',
                '2021-10-06T20:17:16.076372+03:00',
            ],
            'With numeric timezone without delimiter' => [
                '2021-10-06T20:17:16.076372256+0300',
                '2021-10-06T20:17:16.076372+0300',
            ],
            'With numeric short timezone' => [
                '2021-10-06T20:17:16.076372256+03',
                '2021-10-06T20:17:16.076372+03',
            ],
        ];
    }
}
