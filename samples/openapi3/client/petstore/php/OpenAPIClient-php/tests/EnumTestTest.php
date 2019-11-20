<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\EnumTest;
use PHPUnit\Framework\TestCase;

class EnumTestTest extends TestCase
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
