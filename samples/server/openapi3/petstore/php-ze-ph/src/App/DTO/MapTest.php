<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class MapTest
{
    /**
     * @DTA\Data(field="map_map_of_string", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":map[string,string]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":map[string,string]::class}}
     * }})
     * @var map[string,map[string,string]]
     */
    public $map_map_of_string;
    /**
     * @DTA\Data(field="map_of_enum_string", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var map[string,string]
     */
    public $map_of_enum_string;
    /**
     * @DTA\Data(field="direct_map", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"bool"}}
     * }})
     * @var map[string,bool]
     */
    public $direct_map;
    /**
     * @DTA\Data(field="indirect_map", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"bool"}}
     * }})
     * @var map[string,bool]
     */
    public $indirect_map;
}
