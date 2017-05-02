<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class ArrayOfArrayOfNumberOnly 
{
    /**
     * @DTA\Data(field="ArrayArrayNumber", nullable=true)
     * @DTA\Strategy(name="ObjectArray", options={"type":float[]::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":float[]::class}}
     * }})
     * @var float[][]
     */
    public $array_array_number;
}

