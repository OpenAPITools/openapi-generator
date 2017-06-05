<?php

namespace Swagger\Client;

use Swagger\Client\Model\FormatTest;

class DateTimeSerializerTest extends \PHPUnit_Framework_TestCase
{
    public function testDateTimeSanitazion()
    {
        $dateTime = new \DateTime('April 30, 1973 17:05 CEST');

        $input = new FormatTest([
            'date_time' => $dateTime,
        ]);

        $data = ObjectSerializer::sanitizeForSerialization($input);

        $this->assertEquals($data->dateTime, '1973-04-30T17:05:00+02:00');
    }

    public function testDateSanitazion()
    {
        $dateTime = new \DateTime('April 30, 1973 17:05 CEST');

        $input = new FormatTest([
            'date' => $dateTime,
        ]);

        $data = ObjectSerializer::sanitizeForSerialization($input);

        $this->assertEquals($data->date, '1973-04-30');
    }
}
