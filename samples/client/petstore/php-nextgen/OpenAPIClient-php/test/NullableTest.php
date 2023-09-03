<?php

namespace OpenAPI\Client;

use InvalidArgumentException;
use OpenAPI\Client\Model\Name;
use OpenAPI\Client\Model\NullableClass;
use PHPUnit\Framework\TestCase;

/**
 * class NullableTest
 *
 * @package OpenAPI\Client
 */
class NullableTest extends TestCase
{
    public function testNotNullableException(): void
    {
        $name = new Name();
        $name->setName(1);
        $this->assertEquals(1, $name->getName(), 'Non-nullable property can be set and retains its value');

        $this->expectException(InvalidArgumentException::class);
        $this->expectExceptionMessage('non-nullable name cannot be null');

        $name->setName(null);
    }

    public function testNullableobject(): void
    {
        $nullable = new NullableClass();

        $nullable->setIntegerProp(null);
        $this->assertNull($nullable->getIntegerProp(), 'Nullable property can be set to null retains its value');

        $nullable->setIntegerProp(1);
        $this->assertEquals(1, $nullable->getIntegerProp(), 'Nullable property can be set and retains its value');
    }
}