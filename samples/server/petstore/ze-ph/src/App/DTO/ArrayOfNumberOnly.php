<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class ArrayOfNumberOnly 
{
    /**
     * @DTA\Data(field="ArrayNumber", nullable=true)
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"float"}}
     * }})
     * @var float[]
     */
    public $array_number;
}

