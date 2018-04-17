<?php

namespace Swagger\Client;

use Swagger\Client\Model\EnumClass;

class EnumClassTest extends \PHPUnit_Framework_TestCase
{
    public function testPossibleValues()
    {
        $this->assertSame(EnumClass::ABC, '_abc');
        $this->assertSame(EnumClass::EFG, '-efg');
        $this->assertSame(EnumClass::XYZ, '(xyz)');
    }
}
