<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class ArrayOfArrayOfNumberOnly
{
    /**
     * @DTA\Data(field="ArrayArrayNumber", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":float[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":float[]::class}}
     * }})
     * @var float[][]
     */
    public $array_array_number;
}
