<?php

namespace Swagger\Client;

use Swagger\Client\Model\EnumTest;
use Swagger\Client\Model\OuterEnum;

class OuterEnumTest extends \PHPUnit_Framework_TestCase
{
    public function testDeserialize()
    {
        $result = ObjectSerializer::deserialize(
            "placed",
            OuterEnum::class
        );

        $this->assertInternalType('string', $result);
        $this->assertEquals('placed', $result);
    }

    public function testDeserializeInvalidValue()
    {
        $this->setExpectedException(\InvalidArgumentException::class, 'Invalid value for enum');

        ObjectSerializer::deserialize(
            "lkjfalgkdfjg",
            OuterEnum::class
        );
    }

    public function testDeserializeNested()
    {
        $json = '{
            "enum_string": "UPPER",
            "enum_integer": -1,
            "enum_number": -1.2, 
            "outerEnum": "approved"
        }';

        /** * @var EnumTest $result */
        $result = ObjectSerializer::deserialize(
            json_decode($json),
            EnumTest::class
        );

        $this->assertInstanceOf(EnumTest::class, $result);
        $this->assertEquals('approved', $result->getOuterEnum());
    }

    public function testSanitize()
    {
        $json = "placed";

        $result = ObjectSerializer::sanitizeForSerialization(
            $json
        );

        $this->assertInternalType('string', $result);
    }

    public function testSanitizeNested()
    {
        $input = new EnumTest([
            'enum_string' => 'UPPER',
            'enum_integer' => -1,
            'enum_number' => -1.2,
            'outer_enum' => 'approved'
        ]);

        $result = ObjectSerializer::sanitizeForSerialization(
            $input
        );

        $this->assertInternalType('object', $result);
        $this->assertInstanceOf(\stdClass::class, $result);

        $this->assertInternalType('string', $result->outerEnum);
        $this->assertEquals('approved', $result->outerEnum);
    }

    public function testSanitizeNestedInvalidValue()
    {
        $this->setExpectedException(\InvalidArgumentException::class, 'Invalid value for enum');

        $input = new EnumTest([
            'enum_string' => 'UPPER',
            'enum_integer' => -1,
            'enum_number' => -1.2,
            'outer_enum' => 'invalid_value'
        ]);

        ObjectSerializer::sanitizeForSerialization($input);
    }
}
