<?php

namespace OpenAPI\Client;

use DateTime;
use GuzzleHttp\Psr7\Utils;
use OpenAPI\Client\Model\Pet;
use OpenAPI\Client\Model\Tag;
use PHPUnit\Framework\TestCase;
use SplFileObject;

/**
 * class ObjectSerializerTest
 *
 * @package OpenAPI\Client
 */
class ObjectSerializerTest extends TestCase
{
    /**
     * Test sanitizeFilename
     *
     * @covers ObjectSerializer::sanitizeFilename
     */
    public function testSanitizeFilename(): void
    {
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("sun.gif"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("../sun.gif"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("/var/tmp/sun.gif"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("./sun.gif"));
        
        $this->assertSame("sun", ObjectSerializer::sanitizeFilename("sun"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("..\sun.gif"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("\var\tmp\sun.gif"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename("c:\var\tmp\sun.gif"));
        $this->assertSame("sun.gif", ObjectSerializer::sanitizeFilename(".\sun.gif"));
    }

    /**
     * Test SplFileObject class deserialization.
     *
     * @see https://github.com/OpenAPITools/openapi-generator/pull/11184
     * @covers ObjectSerializer::deserialize
     * @dataProvider provideFileStreams
     */
    public function testDeserializeFile($stream, ?array $httpHeaders = null, ?string $expectedFilename = null): void
    {
        $s = new ObjectSerializer();

        /** @var SplFileObject $file */
        $file = ObjectSerializer::deserialize($stream, '\SplFileObject', $httpHeaders);
        $this->assertInstanceOf(SplFileObject::class, $file);

        if (is_string($expectedFilename)) {
            $this->assertEquals($expectedFilename, $file->getFilename());
        } else {
            $this->assertNotEquals($expectedFilename, $file->getFilename());
        }
    }

    /**
     * File Streams Provider
     * @return array[]
     */
    public function provideFileStreams(): array
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
        $this->assertEquals(new DateTime($expected), $dateTime);
    }

    /**
     * Timestamps provider
     *
     * @return string[][]
     */
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

    /**
     * @covers ObjectSerializer::toQueryValue
     * @dataProvider provideQueryParams
     */
    public function testToQueryValue(
        $data,
        string $paramName,
        string $openApiType,
        string $style,
        bool $explode,
        bool $required,
        $expected
    ): void {
        $value = ObjectSerializer::toQueryValue($data, $paramName, $openApiType, $style, $explode, $required);
        $query = ObjectSerializer::buildQuery($value);
        $this->assertEquals($expected, $query);
    }

    /**
     * Query params provider
     *
     * @return array[]
     */
    public function provideQueryParams(): array
    {
        $array = ['blue', 'black', 'brown'];
        $object = ['R' => 100, 'G' => 200, 'B' => 150];

        return [
            // style form
            // skipValidation
            'form boolean, explode on, required false' => [
                false, 'skipValidation', 'boolean', 'form', true, false, 'skipValidation=0',
            ],
            'form empty boolean, explode on, required false' => [
                null, 'skipValidation', 'boolean', 'form', true, false, '',
            ],
            'form empty boolean, explode on, required true' => [
                null, 'skipValidation', 'boolean', 'form', true, true, 'skipValidation=',
            ],
            // color=
            'form empty, explode on, required true' => [
                '', 'color', 'string', 'form', true, true, 'color=',
            ],
            'form empty, explode on, required false' => [
                '', 'color', 'string', 'form', true, false, '',
            ],
            // color=
            'form empty, explode off, required true' => [
                '', 'color', 'string', 'form', false, true, 'color=',
            ],
            // color=blue
            'form string, explode on, required true' => [
                'blue', 'color', 'string', 'form', true, true, 'color=blue',
            ],
            // color=blue
            'form string, explode off, required true' => [
                'blue', 'color', 'string', 'form', false, true, 'color=blue',
            ],
            // color=blue&color=black&color=brown
            'form array, explode on, required true' => [
                $array, 'color', 'array', 'form', true, true, 'color=blue&color=black&color=brown',
            ],
            // color=blue&color=black&color=brown
            'form nested array, explode on, required true' => [
                ['foobar' => $array], 'color', 'array', 'form', true, true, 'color=blue&color=black&color=brown',
            ],
            // color=blue,black,brown
            'form array, explode off, required true' => [
                $array, 'color', 'array', 'form', false, true, 'color=blue%2Cblack%2Cbrown',
            ],
            // color=blue,black,brown
            'form nested array, explode off, required true' => [
                ['foobar' => $array], 'color', 'array', 'form', false, true, 'color=blue%2Cblack%2Cbrown',
            ],
            // R=100&G=200&B=150
            'form object, explode on, required true' => [
                $object, 'color', 'object', 'form', true, true, 'R=100&G=200&B=150',
            ],
            // color=R,100,G,200,B,150
            'form object, explode off, required true' => [
                $object, 'color', 'object', 'form', false, true, 'color=R%2C100%2CG%2C200%2CB%2C150',
            ],

            // SPACE DELIMITED
            // color=blue
            'spaceDelimited primitive, explode off, required true' => [
                'blue', 'color', 'string', 'spaceDelimited', false, true, 'color=blue',
            ],
            // color=blue
            'spaceDelimited primitive, explode on, required true' => [
                'blue', 'color', 'string', 'spaceDelimited', true, true, 'color=blue',
            ],
            // color=blue%20black%20brown
            'spaceDelimited array, explode off, required true' => [
                $array, 'color', 'array', 'spaceDelimited', false, true, 'color=blue%20black%20brown',
            ],
            // color=blue&color=black&color=brown
            'spaceDelimited array, explode on, required true' => [
                $array, 'color', 'array', 'spaceDelimited', true, true, 'color=blue&color=black&color=brown',
            ],
            // color=R%20100%20G%20200%20B%20150
            // swagger editor gives color=R,100,G,200,B,150
            'spaceDelimited object, explode off, required true' => [
                $object, 'color', 'object', 'spaceDelimited', false, true, 'color=R%20100%20G%20200%20B%20150',
            ],
            // R=100&G=200&B=150
            'spaceDelimited object, explode on, required true' => [
                $object, 'color', 'object', 'spaceDelimited', true, true, 'R=100&G=200&B=150',
            ],

            // PIPE DELIMITED
            // color=blue
            'pipeDelimited primitive, explode off, required true' => [
                'blue', 'color', 'string', 'pipeDelimited', false, true, 'color=blue',
            ],
            // color=blue
            'pipeDelimited primitive, explode on, required true' => [
                'blue', 'color', 'string', 'pipeDelimited', true, true, 'color=blue',
            ],
            // color=blue|black|brown
            'pipeDelimited array, explode off, required true' => [
                $array, 'color', 'array', 'pipeDelimited', false, true, 'color=blue%7Cblack%7Cbrown',
            ],
            // color=blue&color=black&color=brown
            'pipeDelimited array, explode on, required true' => [
                $array, 'color', 'array', 'pipeDelimited', true, true, 'color=blue&color=black&color=brown',
            ],
            // color=R|100|G|200|B|150
            // swagger editor gives color=R,100,G,200,B,150
            'pipeDelimited object, explode off, required true' => [
                $object, 'color', 'object', 'pipeDelimited', false, true, 'color=R%7C100%7CG%7C200%7CB%7C150',
            ],
            // R=100&G=200&B=150
            'pipeDelimited object, explode on, required true' => [
                $object, 'color', 'object', 'pipeDelimited', true, true, 'R=100&G=200&B=150',
            ],

            // DEEP OBJECT
            // color=blue
            'deepObject primitive, explode off, required true' => [
                'blue', 'color', 'string', 'deepObject', false, true, 'color=blue',
            ],
            'deepObject primitive, explode on, required true' => [
                'blue', 'color', 'string', 'deepObject', true, true, 'color=blue',
            ],
            // color=blue,black,brown
            'deepObject array, explode off, required true' => [
                $array, 'color', 'array', 'deepObject', false, true, 'color=blue%2Cblack%2Cbrown',
            ],
            // color=blue&color=black&color=brown
            'deepObject array, explode on, required true' => [
                $array, 'color', 'array', 'deepObject', true, true, 'color=blue&color=black&color=brown',
            ],
            // color[R]=100&color[G]=200&color[B]=150
            'deepObject object, explode off, required true' => [
                $object, 'color', 'object', 'deepObject', false, true, 'color%5BR%5D=100&color%5BG%5D=200&color%5BB%5D=150',
            ],
            // color[R]=100&color[G]=200&color[B]=150
            'deepObject object, explode on, required true' => [
                $object, 'color', 'object', 'deepObject', true, true, 'color%5BR%5D=100&color%5BG%5D=200&color%5BB%5D=150',
            ],
            // filter[or][0][name]=John&filter[or][1][email]=john@doe.com
            'example from @nadar' => [
                [
                    'or' =>
                    [
                        ['name' => 'John'],
                        ['email' => 'john@doe.com'],
                    ],
                ],
                'filter',
                'object',
                'deepObject',
                true,
                true,
                'filter%5Bor%5D%5B0%5D%5Bname%5D=John&filter%5Bor%5D%5B1%5D%5Bemail%5D=john%40doe.com'
            ],
            'form DateTime object, explode on, required true' => [
                new DateTime('2021-10-06T20:17:16'), 'dateTime', '\DateTime', 'form', true, true, 'dateTime=2021-10-06T20%3A17%3A16%2B00%3A00',
            ],
            'form null DateTime object, explode on, required true' => [
                null, 'dateTime', '\DateTime', 'form', true, true, 'dateTime=',
            ],
            'form null DateTime object, explode on, required false' => [
                null, 'dateTime', '\DateTime', 'form', true, false, '',
            ],
            'form 1 int, explode on, required false' => [
                1, 'field', 'int', 'form', true, false, 'field=1',
            ],
            'form 0 int, explode on, required false' => [
                0, 'field', 'int', 'form', true, false, 'field=0',
            ],
            'form 0 int, explode on, required true' => [
                0, 'field', 'int', 'form', true, true, 'field=0',
            ],
            'form null int, explode on, required false' => [
                null, 'field', 'int', 'form', true, false, '',
            ],
            'form null int, explode on, required true' => [
                null, 'field', 'int', 'form', true, true, 'field=',
            ],
            'form 1 integer, explode on, required false' => [
                1, 'field', 'integer', 'form', true, false, 'field=1',
            ],
            'form 0 integer, explode on, required false' => [
                0, 'field', 'integer', 'form', true, false, 'field=0',
            ],
            'form 0 integer, explode on, required true' => [
                0, 'field', 'integer', 'form', true, true, 'field=0',
            ],
            'form null integer, explode on, required false' => [
                null, 'field', 'integer', 'form', true, false, '',
            ],
            'form null integer, explode on, required true' => [
                null, 'field', 'integer', 'form', true, true, 'field=',
            ],
            'form 1.1 float, explode on, required false' => [
                1.1, 'field', 'float', 'form', true, false, 'field=1.1',
            ],
            'form 0 float, explode on, required false' => [
                0, 'field', 'float', 'form', true, false, 'field=0',
            ],
            'form 0.0 float, explode on, required false' => [
                0.0, 'field', 'float', 'form', true, false, 'field=0',
            ],
            'form 0 float, explode on, required true' => [
                0, 'field', 'float', 'form', true, true, 'field=0',
            ],
            'form 0.0 float, explode on, required true' => [
                0.0, 'field', 'float', 'form', true, true, 'field=0',
            ],
            'form null float, explode on, required false' => [
                null, 'field', 'float', 'form', true, false, '',
            ],
            'form null float, explode on, required true' => [
                null, 'field', 'float', 'form', true, true, 'field=',
            ],
            'form 1.1 number, explode on, required false' => [
                1.1, 'field', 'number', 'form', true, false, 'field=1.1',
            ],
            'form 0 number, explode on, required false' => [
                0, 'field', 'number', 'form', true, false, 'field=0',
            ],
            'form 0.0 number, explode on, required false' => [
                0.0, 'field', 'number', 'form', true, false, 'field=0',
            ],
            'form 0 number, explode on, required true' => [
                0, 'field', 'number', 'form', true, true, 'field=0',
            ],
            'form 0.0 number, explode on, required true' => [
                0.0, 'field', 'number', 'form', true, true, 'field=0',
            ],
            'form null number, explode on, required false' => [
                null, 'field', 'number', 'form', true, false, '',
            ],
            'form null number, explode on, required true' => [
                null, 'field', 'number', 'form', true, true, 'field=',
            ],
            'form true bool, explode on, required false' => [
                true, 'field', 'bool', 'form', true, false, 'field=1',
            ],
            'form false bool, explode on, required false' => [
                false, 'field', 'bool', 'form', true, false, 'field=0',
            ],
            'form empty bool, explode on, required false' => [
                null, 'field', 'bool', 'form', true, false, '',
            ],
            'form empty bool, explode on, required true' => [
                null, 'field', 'bool', 'form', true, true, 'field=',
            ],
            # Entries for "boolean" type are already covered in the beginning of this provider
            'form 1 bool, explode on, required false' => [
                1, 'field', 'bool', 'form', true, false, 'field=1',
            ],
            'form 0 bool, explode on, required false' => [
                0, 'field', 'bool', 'form', true, false, 'field=0',
            ],
        ];
    }

    /**
     * @covers ObjectSerializer::toQueryValue
     * @dataProvider provideDeepObjects
     */
    public function testDeepObjectStyleQueryParam(
        $data,
        $paramName,
        $expected
    ): void {
        $value = ObjectSerializer::buildQuery(ObjectSerializer::toQueryValue($data, $paramName, 'object', 'deepObject', true));
        parse_str($value, $result);
        $this->assertEquals($expected, $result);
    }

    /**
     * Deep Objects provider
     *
     * @return array
     */
    public function provideDeepObjects(): array
    {
        return [
            /** example @see https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#style-examples */
            'example OpenAPISpec doc' => [
                ['R' => 100, 'G' => 200, 'B' => 150],
                'color',
                [
                    'color' => [
                        'R' => 100,
                        'G' => 200,
                        'B' => 150,
                    ],
                ],
            ],
            /** example @see https://swagger.io/docs/specification/serialization/ */
            'example Swagger doc' => [
                ['role' => 'admin', 'firstName' => 'Alex'],
                'id',
                [
                    'id' => [
                        'role' => 'admin',
                        'firstName' => 'Alex',
                    ],
                ],
            ],
        ];
    }

    /**
     * @dataProvider provideToStringInput
     */
    public function testToString($input, string $expected): void
    {
        $result = ObjectSerializer::toString($input);

        $this->assertSame($expected, $result);
    }

    /**
     * toString() input provider
     *
     * @return array
     */
    public function provideToStringInput(): array
    {
        return [
            'int' => [
                'input' => 1,
                'expected' => '1',
            ],
            'int 0' => [
                'input' => 0,
                'expected' => '0',
            ],
            'false' => [
                'input' => false,
                'expected' => 'false',
            ],
            'true' => [
                'input' => true,
                'expected' => 'true',
            ],
            'some string' => [
                'input' => 'some string',
                'expected' => 'some string',
            ],
            'a date' => [
                'input' => new DateTime('14-04-2022'),
                'expected' => '2022-04-14T00:00:00+00:00',
            ],
        ];
    }

    /**
     * @covers ObjectSerializer::toQueryValue
     * @dataProvider provideQueryParamsWithStringBooleanFormatForQueryString
     */
    public function testToQueryValueWithStringBooleanFormatForQueryString(
        $data,
        string $paramName,
        string $openApiType,
        string $style,
        bool $explode,
        bool $required,
        $expected
    ): void
    {
        $config = new Configuration();
        $config->setBooleanFormatForQueryString(Configuration::BOOLEAN_FORMAT_STRING);
        $config::setDefaultConfiguration($config);

        $value = ObjectSerializer::toQueryValue($data, $paramName, $openApiType, $style, $explode, $required);
        $query = ObjectSerializer::buildQuery($value);
        $this->assertEquals($expected, $query);
    }

    /**
     * Query Params with string boolean format provider
     *
     * @return array[]
     */
    public function provideQueryParamsWithStringBooleanFormatForQueryString(): array
    {
        return [
            // style form
            // skipValidation
            'form boolean, required false' => [
                false, 'skipValidation', 'boolean', 'form', true, false, 'skipValidation=false',
            ],
            'form empty boolean, required false' => [
                null, 'skipValidation', 'boolean', 'form', true, false, '',
            ],
            'form empty boolean, required true' => [
                null, 'skipValidation', 'boolean', 'form', true, true, 'skipValidation=',
            ],
            'form true boolean, required true' => [
                true, 'skipValidation', 'boolean', 'form', true, false, 'skipValidation=true',
            ],
        ];
    }

    /**
     * Test array to class deserialization.
     *
     * @covers ObjectSerializer::deserialize
     *
     * @see https://github.com/OpenAPITools/openapi-generator/pull/12849#issuecomment-1186130098
     */
    public function testArrayGivenAsObjectForDeserialize(): void
    {
        $data = [
            'name' => 'Pet Name',
            'status' => Pet::STATUS_AVAILABLE,
            'tags' => [
                ['name' => 'Tag Name'],
            ]
        ];

        /** @var Pet $pet */
        $pet = ObjectSerializer::deserialize($data, Pet::class);
        $this->assertEquals('Pet Name', $pet->getName());
        $this->assertEquals(Pet::STATUS_AVAILABLE, $pet->getStatus());

        $tags = $pet->getTags();
        $this->assertIsArray($tags);

        $tag = $tags[0];
        $this->assertInstanceOf(Tag::class, $tag);
    }
}
