<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class AdditionalPropertiesClass 
{
    /**
     * @DTA\Data(field="map_property", nullable=true)
     * @var map[string,string]
     */
    public $map_property;
    /**
     * @DTA\Data(field="map_of_map_property", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":map[string,map[string,string]]::class})
     * @DTA\Validator(name="Dictionary", options={"type":map[string,map[string,string]]::class})
     * @var map[string,map[string,string]]
     */
    public $map_of_map_property;
}

