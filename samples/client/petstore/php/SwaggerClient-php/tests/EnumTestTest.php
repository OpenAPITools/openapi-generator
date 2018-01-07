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
}
