<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class ArrayTest 
{
    /**
     * @DTA\Data(field="array_of_string", nullable=true)
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var string[]
     */
    public $array_of_string;
    /**
     * @DTA\Data(field="array_array_of_integer", nullable=true)
     * @DTA\Strategy(name="ObjectArray", options={"type":int[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":int[]::class}}
     * }})
     * @var int[][]
     */
    public $array_array_of_integer;
    /**
     * @DTA\Data(field="array_array_of_model", nullable=true)
     * @DTA\Strategy(name="ObjectArray", options={"type":\App\DTO\ReadOnlyFirst[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":\App\DTO\ReadOnlyFirst[]::class}}
     * }})
     * @var \App\DTO\ReadOnlyFirst[][]
     */
    public $array_array_of_model;
}

