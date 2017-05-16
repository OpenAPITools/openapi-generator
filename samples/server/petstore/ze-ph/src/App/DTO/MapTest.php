<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class MapTest 
{
    /**
     * @DTA\Data(field="map_map_of_string", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":map[string,map[string,string]]::class})
     * @DTA\Validator(name="Dictionary", options={"type":map[string,map[string,string]]::class})
     * @var map[string,map[string,string]]
     */
    public $map_map_of_string;
    /**
     * @DTA\Data(field="map_of_enum_string", nullable=true)
     * @var map[string,string]
     */
    public $map_of_enum_string;
}

