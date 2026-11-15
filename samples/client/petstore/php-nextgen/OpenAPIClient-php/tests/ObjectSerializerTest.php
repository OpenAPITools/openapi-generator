<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\ArrayTest;
use OpenAPI\Client\Model\Category;
use OpenAPI\Client\Model\Pet;
use OpenAPI\Client\Model\Tag;
use PHPUnit\Framework\TestCase;

/**
 * class ObjectSerializerTest
 *
 * @package OpenAPI\Client
 */
class ObjectSerializerTest extends TestCase
{
    /**
     * @covers ObjectSerializer::toQueryValue
     * @dataProvider provideQueryParams
     */
    public function testToQueryValue(
        mixed $data,
        string $paramName,
        string $openApiType,
        string $style,
        bool $explode,
        bool $required,
        mixed $expected
    ): void {
        $value = ObjectSerializer::toQueryValue($data, $paramName, $openApiType, $style, $explode, $required);
        $query = ObjectSerializer::buildQuery($value);

        $this->assertEquals($expected, $query);
    }

    /**
     * Query params provider
     *
     * Values that are not model instances, whose serialization is unchanged.
     *
     * @return array[]
     */
    public static function provideQueryParams(): array
    {
        $statuses = ['available', 'pending', 'sold'];
        $filter = ['name' => 'Rex', 'status' => 'available'];

        $stdClass = new \stdClass();
        $stdClass->name = 'Rex';
        $stdClass->category = ['name' => 'Dogs'];

        return [
            // style form
            // status=available&status=pending&status=sold
            'form array, explode on, required true' => [
                $statuses, 'status', 'array', 'form', true, true, 'status=available&status=pending&status=sold',
            ],
            // status=available,pending,sold
            'form array, explode off, required true' => [
                $statuses, 'status', 'array', 'form', false, true, 'status=available%2Cpending%2Csold',
            ],
            // name=Rex&status=available
            'form object, explode on, required true' => [
                $filter, 'filter', 'object', 'form', true, true, 'name=Rex&status=available',
            ],
            // filter=name,Rex,status,available
            'form object, explode off, required true' => [
                $filter, 'filter', 'object', 'form', false, true, 'filter=name%2CRex%2Cstatus%2Cavailable',
            ],
            // status=available
            'form string, explode on, required true' => [
                'available', 'status', 'string', 'form', true, true, 'status=available',
            ],
            // quantity=0
            'form 0 integer, explode on, required false' => [
                0, 'quantity', 'integer', 'form', true, false, 'quantity=0',
            ],
            // enabled=true
            'form true boolean, explode on, required true' => [
                true, 'enabled', 'boolean', 'form', true, true, 'enabled=true',
            ],
            // enabled=false
            'form false boolean, explode on, required false' => [
                false, 'enabled', 'boolean', 'form', true, false, 'enabled=false',
            ],

            // DEEP OBJECT
            // status[0]=available&status[1]=pending&status[2]=sold
            'deepObject array, explode on, required true' => [
                $statuses, 'status', 'array', 'deepObject', true, true,
                'status%5B0%5D=available&status%5B1%5D=pending&status%5B2%5D=sold',
            ],
            // filter[name]=Rex&filter[status]=available
            'deepObject object, explode on, required true' => [
                $filter, 'filter', 'object', 'deepObject', true, true,
                'filter%5Bname%5D=Rex&filter%5Bstatus%5D=available',
            ],
            // filter[name]=Rex&filter[category][name]=Dogs
            'deepObject stdClass, explode on, required true' => [
                $stdClass, 'filter', 'object', 'deepObject', true, true,
                'filter%5Bname%5D=Rex&filter%5Bcategory%5D%5Bname%5D=Dogs',
            ],
        ];
    }

    /**
     * @covers ObjectSerializer::buildQuery
     */
    public function testBuildQueryWithRawBooleans(): void
    {
        $this->assertEquals(
            'enabled=true&disabled=false&list=true&list=false',
            ObjectSerializer::buildQuery(['enabled' => true, 'disabled' => false, 'list' => [true, false]])
        );
    }

    /**
     * The deprecated int format must still serialize booleans as 1 or 0.
     *
     * @covers ObjectSerializer::toQueryValue
     * @covers ObjectSerializer::buildQuery
     */
    public function testBooleanFormatInt(): void
    {
        $defaultConfiguration = Configuration::getDefaultConfiguration();
        Configuration::setDefaultConfiguration(
            (new Configuration())->setBooleanFormatForQueryString(Configuration::BOOLEAN_FORMAT_INT)
        );

        try {
            $value = ObjectSerializer::toQueryValue(true, 'enabled', 'boolean', 'form', true, true);
            $this->assertEquals('enabled=1', ObjectSerializer::buildQuery($value));
            $this->assertEquals('enabled=0', ObjectSerializer::buildQuery(['enabled' => false]));
        } finally {
            Configuration::setDefaultConfiguration($defaultConfiguration);
        }
    }

    /**
     * The string format must serialize booleans as true or false.
     *
     * @covers ObjectSerializer::toQueryValue
     * @covers ObjectSerializer::buildQuery
     */
    public function testBooleanFormatString(): void
    {
        $defaultConfiguration = Configuration::getDefaultConfiguration();
        Configuration::setDefaultConfiguration(
            (new Configuration())->setBooleanFormatForQueryString(Configuration::BOOLEAN_FORMAT_STRING)
        );

        try {
            $value = ObjectSerializer::toQueryValue(true, 'enabled', 'boolean', 'form', true, true);
            $this->assertEquals('enabled=true', ObjectSerializer::buildQuery($value));
            $this->assertEquals('enabled=false', ObjectSerializer::buildQuery(['enabled' => false]));
        } finally {
            Configuration::setDefaultConfiguration($defaultConfiguration);
        }
    }

    /**
     * An object-typed query parameter given as a model must serialize to the model's
     * values, read through its getters and keyed by its attributeMap - note photoUrls.
     *
     * @see https://github.com/OpenAPITools/openapi-generator/issues/11222
     * @covers ObjectSerializer::toQueryValue
     */
    public function testToQueryValueWithModelInstance(): void
    {
        // photo_urls is required and non-nullable, so it must be set for its getter to return.
        $pet = new Pet([
            'id' => 1,
            'name' => 'Rex',
            'photo_urls' => ['a.png', 'b.png'],
            'status' => 'available',
        ]);

        $query = ObjectSerializer::toQueryValue($pet, 'filter', 'object', 'deepObject', true, false);

        $this->assertEquals(
            'filter[id]=1&filter[name]=Rex&filter[photoUrls][0]=a.png'
                . '&filter[photoUrls][1]=b.png&filter[status]=available',
            urldecode(ObjectSerializer::buildQuery($query))
        );
    }

    /**
     * A composed (allOf/oneOf/anyOf) parameter is typed with a model name rather than
     * "object", so models are recognised by their interface instead of that type.
     *
     * @covers ObjectSerializer::toQueryValue
     * @dataProvider provideModelTypedStyles
     */
    public function testToQueryValueWithModelInstanceTypedByItsModelName(
        string $style,
        string $expected
    ): void {
        $pet = new Pet(['id' => 1, 'name' => 'Rex', 'photo_urls' => [], 'status' => 'available']);

        $query = ObjectSerializer::toQueryValue($pet, 'filter', Pet::class, $style, true, false);

        $this->assertSame($expected, urldecode(ObjectSerializer::buildQuery($query)));
    }

    /**
     * Styles provider for a parameter typed with a model name
     *
     * @return array[]
     */
    public static function provideModelTypedStyles(): array
    {
        return [
            'deepObject, explode on' => [
                'deepObject', 'filter[id]=1&filter[name]=Rex&filter[status]=available',
            ],
            'form, explode on' => [
                'form', 'id=1&name=Rex&status=available',
            ],
        ];
    }

    /**
     * A model holding no values flattens to nothing, so the parameter is dropped instead
     * of being sent as an empty container key.
     *
     * A deepObject has no key of its own to fall back on, so a required parameter is
     * dropped just the same - hence the identical result for both.
     *
     * @covers ObjectSerializer::toQueryValue
     */
    public function testToQueryValueWithAnEmptyModelInstance(): void
    {
        $this->assertSame(
            [],
            ObjectSerializer::toQueryValue(new Tag(), 'filter', Tag::class, 'deepObject', true, false)
        );
        $this->assertSame(
            [],
            ObjectSerializer::toQueryValue(new Tag(), 'filter', Tag::class, 'deepObject', true, true)
        );
    }

    /**
     * Objects nested in a plain object are converted as well: a shallow (array) cast would
     * leave them in place, and building the query would then fail to stringify them.
     *
     * @covers ObjectSerializer::toQueryValue
     */
    public function testToQueryValueWithAPlainObjectHoldingNestedObjects(): void
    {
        $filter = new \stdClass();
        $filter->name = 'Rex';
        $filter->tag = new Tag(['id' => 2, 'name' => 'stray']);
        $filter->bornAt = new \DateTime('2024-01-02T03:04:05Z');

        $query = ObjectSerializer::toQueryValue($filter, 'filter', 'object', 'deepObject', true, false);

        $this->assertSame(
            'filter[name]=Rex&filter[tag][id]=2&filter[tag][name]=stray'
                . '&filter[bornAt]=2024-01-02T03:04:05+00:00',
            urldecode(ObjectSerializer::buildQuery($query))
        );
    }

    /**
     * Array properties keep their indexes, at every depth.
     *
     * @covers ObjectSerializer::toQueryValue
     */
    public function testToQueryValueWithArrayPropertiesOfAModelInstance(): void
    {
        $model = new ArrayTest([
            'array_of_string' => ['a', 'b'],
            'array_array_of_integer' => [[1, 2]],
        ]);

        $query = ObjectSerializer::toQueryValue($model, 'filter', 'object', 'deepObject', true, false);

        $this->assertEquals(
            'filter[array_of_string][0]=a&filter[array_of_string][1]=b'
                . '&filter[array_array_of_integer][0][0]=1&filter[array_array_of_integer][0][1]=2',
            urldecode(ObjectSerializer::buildQuery($query))
        );
    }

    /**
     * Nested models must be flattened too, so the whole object graph is converted to
     * arrays first: a shallow cast would leave them as objects and fail to stringify.
     *
     * @covers ObjectSerializer::toQueryValue
     */
    public function testToQueryValueWithNestedModelInstances(): void
    {
        $pet = new Pet([
            'id' => 1,
            'name' => 'Rex',
            'photo_urls' => [],
            'category' => new Category(['id' => 7, 'name' => 'Dogs']),
            'tags' => [new Tag(['id' => 2, 'name' => 'cute'])],
        ]);

        $query = ObjectSerializer::toQueryValue($pet, 'filter', 'object', 'deepObject', true, false);

        $this->assertEquals(
            'filter[id]=1&filter[category][id]=7&filter[category][name]=Dogs'
                . '&filter[name]=Rex&filter[tags][0][id]=2&filter[tags][0][name]=cute',
            urldecode(ObjectSerializer::buildQuery($query))
        );
    }

    /**
     * Boolean query values must be formatted like the configuration that is passed,
     * e.g. the configuration of an API instance, instead of the default configuration.
     *
     * @covers ObjectSerializer::toQueryValue
     * @covers ObjectSerializer::convertBoolToQueryStringFormat
     * @covers ObjectSerializer::buildQuery
     */
    public function testBooleanQueryValuesUseFormatOfGivenConfiguration(): void
    {
        $defaultConfiguration = Configuration::getDefaultConfiguration();
        $defaultFormat = $defaultConfiguration->getBooleanFormatForQueryString();
        $defaultConfiguration->setBooleanFormatForQueryString(Configuration::BOOLEAN_FORMAT_INT);
        $config = (new Configuration())->setBooleanFormatForQueryString(Configuration::BOOLEAN_FORMAT_STRING);

        try {
            $this->assertSame(
                ['flag' => 'true'],
                ObjectSerializer::toQueryValue(true, 'flag', 'boolean', 'form', true, true, $config)
            );
            $this->assertSame('false', ObjectSerializer::convertBoolToQueryStringFormat(false, $config));
            $this->assertSame(
                'flag=true&flags=true&flags=false',
                ObjectSerializer::buildQuery(['flag' => true, 'flags' => [true, false]], PHP_QUERY_RFC3986, $config)
            );

            // without a configuration, the default configuration is used
            $this->assertSame(['flag' => 1], ObjectSerializer::toQueryValue(true, 'flag', 'boolean'));
            $this->assertSame('flag=1', ObjectSerializer::buildQuery(['flag' => true]));
        } finally {
            $defaultConfiguration->setBooleanFormatForQueryString($defaultFormat);
        }
    }

    /**
     * Booleans in non-exploded query values must be formatted like the given configuration,
     * instead of being imploded into "1" and "".
     *
     * @covers ObjectSerializer::toQueryValue
     * @dataProvider provideNonExplodedBooleanQueryValues
     */
    public function testNonExplodedBooleanQueryValues(
        mixed $data,
        string $openApiType,
        string $style,
        string $booleanFormat,
        string $expected
    ): void {
        $config = (new Configuration())->setBooleanFormatForQueryString($booleanFormat);

        $value = ObjectSerializer::toQueryValue($data, 'flags', $openApiType, $style, false, true, $config);

        $this->assertSame($expected, urldecode(ObjectSerializer::buildQuery($value, PHP_QUERY_RFC3986, $config)));
    }

    public static function provideNonExplodedBooleanQueryValues(): array
    {
        return [
            'form array, string format' => [
                [true, false], 'array', 'form', Configuration::BOOLEAN_FORMAT_STRING, 'flags=true,false',
            ],
            'form array, int format' => [
                [true, false], 'array', 'form', Configuration::BOOLEAN_FORMAT_INT, 'flags=1,0',
            ],
            'pipeDelimited array, string format' => [
                [true, false], 'array', 'pipeDelimited', Configuration::BOOLEAN_FORMAT_STRING, 'flags=true|false',
            ],
            'form object, string format' => [
                ['enabled' => true, 'archived' => false], 'object', 'form', Configuration::BOOLEAN_FORMAT_STRING,
                'flags=enabled,true,archived,false',
            ],
            'form boolean, int format' => [
                false, 'boolean', 'form', Configuration::BOOLEAN_FORMAT_INT, 'flags=0',
            ],
        ];
    }
}
