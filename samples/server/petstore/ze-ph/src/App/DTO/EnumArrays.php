<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class EnumArrays 
{
    /**
     * @DTA\Data(field="just_symbol", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $just_symbol;
    /**
     * @DTA\Data(field="array_enum", nullable=true)
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var string[]
     */
    public $array_enum;
}

