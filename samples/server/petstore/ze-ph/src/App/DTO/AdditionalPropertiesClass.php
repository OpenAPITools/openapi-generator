<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class AdditionalPropertiesClass
{
    /**
     * @DTA\Data(field="map_property", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var map[string,string]
     */
    public $map_property;
    /**
     * @DTA\Data(field="map_of_map_property", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":map[string,string]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":map[string,string]::class}}
     * }})
     * @var map[string,map[string,string]]
     */
    public $map_of_map_property;
}
