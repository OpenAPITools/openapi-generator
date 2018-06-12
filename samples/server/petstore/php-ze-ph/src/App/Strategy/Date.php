<?php

namespace App\Strategy;

class Date extends DateTime
{
    const DATE_TIME_FORMAT = 'Y-m-d';

    /**
     * @param $arrayValue
     * @return \DateTime
     */
    protected function parseDateString($arrayValue)
    {
        return \DateTime::createFromFormat(
            static::DATE_TIME_FORMAT . ' H:i:sP',
            $arrayValue . ' 00:00:00+00:00',
            new \DateTimeZone('UTC')
        );
    }
}
