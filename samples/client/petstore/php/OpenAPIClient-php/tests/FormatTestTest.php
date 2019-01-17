<?php

use OpenAPI\Client\Model\FormatTest;
use PHPUnit\Framework\TestCase;

class FormatTestTest extends TestCase
{
    public function testCountTheLengthOfMultiByteStringsCorrectly()
    {
        $the64MultiByteStrings = '１２３４５６７８９０１２３４５６７８９０１２３４５６７８９０１２３４５６７８９０１２３４５６７８９０１２３４５６７８９０１２３';

        // Pass the string via constructor.
        $formatTest = new FormatTest([
            'password' => $the64MultiByteStrings,
            // mandatory parameters
            'number' => 500,
            'byte' => base64_encode('test'),
            'date' => new DateTime(),
        ]);

        $this->assertEmpty($formatTest->listInvalidProperties());

        // Pass the strings via setter.
        // Throws InvalidArgumentException if it doesn't count the length correctly.
        $formatTest->setPassword($the64MultiByteStrings);
    }
}
