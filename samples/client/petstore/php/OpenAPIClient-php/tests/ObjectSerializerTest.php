<?php

namespace OpenAPI\Client;

use GuzzleHttp\Psr7\Utils;
use PHPUnit\Framework\TestCase;
use Psr\Http\Message\StreamInterface;

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
    public function testDeserializeFile(StreamInterface $stream, ?array $httpHeaders = null, ?string $expectedFilename = null): void
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
        ];
    }
}
