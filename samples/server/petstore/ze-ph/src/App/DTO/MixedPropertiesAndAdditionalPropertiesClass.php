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
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":\App\DTO\Animal::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":\App\DTO\Animal::class}}
     * }})
     * @var map[string,\App\DTO\Animal]
     */
    public $map;
}
