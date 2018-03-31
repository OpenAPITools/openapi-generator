<?php

namespace Swagger\Client;

use Swagger\Client\Model\EnumTest;

class EnumTestTest extends \PHPUnit_Framework_TestCase
{
    public function testPossibleValues()
    {
        $this->assertSame(EnumTest::ENUM_STRING_UPPER, "UPPER");
        $this->assertSame(EnumTest::ENUM_STRING_LOWER, "lower");
        $this->assertSame(EnumTest::ENUM_INTEGER_1, 1);
        $this->assertSame(EnumTest::ENUM_INTEGER_MINUS_1, -1);
        $this->assertSame(EnumTest::ENUM_NUMBER_1_DOT_1, 1.1);
        $this->assertSame(EnumTest::ENUM_NUMBER_MINUS_1_DOT_2, -1.2);
    }

    public function testStrictValidation()
    {
        $enum = new EnumTest([
            'enum_string' => 0,
        ]);

        $this->assertFalse($enum->valid());

        $expected = [
            "invalid value for 'enum_string', must be one of 'UPPER', 'lower', ''",
            "'enum_string_required' can't be null",
        ];
        $this->assertSame($expected, $enum->listInvalidProperties());
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testThrowExceptionWhenInvalidAmbiguousValueHasPassed()
    {
        $enum = new EnumTest();
        $enum->setEnumString(0);
    }

    public function testNonRequiredPropertyIsOptional()
    {
        $enum = new EnumTest([
            'enum_string_required' => 'UPPER',
        ]);
        $this->assertSame([], $enum->listInvalidProperties());
        $this->assertTrue($enum->valid());
    }

    public function testRequiredProperty()
    {
        $enum = new EnumTest();
        $this->assertSame(["'enum_string_required' can't be null"], $enum->listInvalidProperties());
        $this->assertFalse($enum->valid());
    }
}
