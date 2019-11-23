<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\FormatTest;
use PHPUnit\Framework\TestCase;

class DateTimeSerializerTest extends TestCase
{
    public function testDateTimeSanitazion()
    {
        $dateTime = new \DateTime('April 30, 1973 17:05 CEST');

        $input = new FormatTest([
            'date_time' => $dateTime,
        ]);

        $data = ObjectSerializer::sanitizeForSerialization($input);

        $this->assertEquals($data->dateTime, '1973-04-30T17:05:00+02:00');

        ObjectSerializer::setDateTimeFormat(\DateTime::RFC3339_EXTENDED);
        $dataFraction = ObjectSerializer::sanitizeForSerialization($input);
        $this->assertEquals($dataFraction->dateTime, '1973-04-30T17:05:00.000+02:00');
        ObjectSerializer::setDateTimeFormat(\DateTime::ATOM);
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
