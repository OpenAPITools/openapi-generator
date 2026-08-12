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
}
