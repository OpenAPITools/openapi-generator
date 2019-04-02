<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class AdditionalPropertiesClass
{
    /**
     * @DTA\Data(field="map_string", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var map[string,string]
     */
    public $map_string;
    /**
     * @DTA\Data(field="map_number", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"float"}}
     * }})
     * @var map[string,float]
     */
    public $map_number;
    /**
     * @DTA\Data(field="map_integer", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var map[string,int]
     */
    public $map_integer;
    /**
     * @DTA\Data(field="map_boolean", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"bool"}}
     * }})
     * @var map[string,bool]
     */
    public $map_boolean;
    /**
     * @DTA\Data(field="map_array_integer", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":int[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":int[]::class}}
     * }})
     * @var map[string,int[]]
     */
    public $map_array_integer;
    /**
     * @DTA\Data(field="map_array_anytype", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":object[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":object[]::class}}
     * }})
     * @var map[string,object[]]
     */
    public $map_array_anytype;
    /**
     * @DTA\Data(field="map_map_string", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":map[string,string]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":map[string,string]::class}}
     * }})
     * @var map[string,map[string,string]]
     */
    public $map_map_string;
    /**
     * @DTA\Data(field="map_map_anytype", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":map[string,object]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":map[string,object]::class}}
     * }})
     * @var map[string,map[string,object]]
     */
    public $map_map_anytype;
    /**
     * @DTA\Data(field="anytype_1", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":object::class})
     * @DTA\Validator(name="Dictionary", options={"type":object::class})
     * @var object
     */
    public $anytype_1;
    /**
     * @DTA\Data(field="anytype_2", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":object::class})
     * @DTA\Validator(name="Dictionary", options={"type":object::class})
     * @var object
     */
    public $anytype_2;
    /**
     * @DTA\Data(field="anytype_3", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":object::class})
     * @DTA\Validator(name="Dictionary", options={"type":object::class})
     * @var object
     */
    public $anytype_3;
}
