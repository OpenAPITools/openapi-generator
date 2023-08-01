<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\FormatTest;
use PHPUnit\Framework\TestCase;

class DateTimeSerializerTest extends TestCase
{
    public function testDateTimeSanitation()
    {
        $dateTime = new \DateTime('April 30, 1973 17:05 CEST');

        $input = new FormatTest([
            'date_time' => $dateTime,
        ]);

        $data = ObjectSerializer::sanitizeForSerialization($input);

        $this->assertEquals('1973-04-30T17:05:00+02:00', $data->dateTime);

        ObjectSerializer::setDateTimeFormat(\DateTime::RFC3339_EXTENDED);
        $dataFraction = ObjectSerializer::sanitizeForSerialization($input);
        $this->assertEquals('1973-04-30T17:05:00.000+02:00', $dataFraction->dateTime);
        ObjectSerializer::setDateTimeFormat(\DateTime::ATOM);
    }

    public function testDateSanitation()
    {
        $dateTime = new \DateTime('April 30, 1973 17:05 CEST');

        $input = new FormatTest([
            'date' => $dateTime,
        ]);

        $data = ObjectSerializer::sanitizeForSerialization($input);

        $this->assertEquals('1973-04-30', $data->date);
    }
}
