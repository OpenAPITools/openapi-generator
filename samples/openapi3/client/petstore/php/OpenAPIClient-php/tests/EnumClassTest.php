<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\EnumClass;
use PHPUnit\Framework\TestCase;

class EnumClassTest extends TestCase
{
    public function testPossibleValues()
    {
        $this->assertSame(EnumClass::ABC, '_abc');
        $this->assertSame(EnumClass::EFG, '-efg');
        $this->assertSame(EnumClass::XYZ, '(xyz)');
    }
}
