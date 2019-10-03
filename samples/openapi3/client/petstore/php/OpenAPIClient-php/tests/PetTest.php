<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\Pet;
use PHPUnit\Framework\TestCase;

class PetTest extends TestCase
{
    /**
     * test empty object serialization
     */
    public function testEmptyPetSerialization()
    {
        $new_pet = new Pet;
        // the empty object should be serialised to {}
        $this->assertSame("{}", "$new_pet");
    }
}
