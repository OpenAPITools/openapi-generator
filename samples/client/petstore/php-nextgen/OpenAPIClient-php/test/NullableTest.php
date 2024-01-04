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

        // comment out below as strict type is now enabled
        //$this->expectException(InvalidArgumentException::class);
        //$this->expectExceptionMessage('non-nullable name cannot be null');

        $this->expectException(TypeError::class);
        $this->expectExceptionMessage('must be of type int, null given');

        //Failed asserting that exception of type "TypeError" matches expected exception "InvalidArgumentException". Message was: "OpenAPI\Client\Model\Name::setName(): Argument #1 ($name) must be of type int, null given, called in /Users/williamcheng/Code/openapi-generator7/samples/client/petstore/php-nextgen/OpenAPIClient-php/test/NullableTest.php on line 26" at
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
