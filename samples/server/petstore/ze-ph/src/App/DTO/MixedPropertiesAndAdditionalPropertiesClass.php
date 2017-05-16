<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class MixedPropertiesAndAdditionalPropertiesClass 
{
    /**
     * @DTA\Data(field="uuid", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $uuid;
    /**
     * @DTA\Data(field="dateTime", nullable=true)
     * @DTA\Strategy(name="DateTime")
     * @DTA\Validator(name="Date", options={"format": \DateTime::RFC3339})
     * @var \DateTime
     */
    public $date_time;
    /**
     * @DTA\Data(field="map", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":map[string,\App\DTO\Animal]::class})
     * @DTA\Validator(name="Dictionary", options={"type":map[string,\App\DTO\Animal]::class})
     * @var map[string,\App\DTO\Animal]
     */
    public $map;
}

