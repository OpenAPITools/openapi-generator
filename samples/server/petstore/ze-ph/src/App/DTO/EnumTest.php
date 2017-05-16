<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class EnumTest 
{
    /**
     * @DTA\Data(field="enum_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $enum_string;
    /**
     * @DTA\Data(field="enum_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $enum_integer;
    /**
     * @DTA\Data(field="enum_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var double
     */
    public $enum_number;
    /**
     * @DTA\Data(field="outerEnum", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterEnum::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterEnum::class})
     * @var \App\DTO\OuterEnum
     */
    public $outer_enum;
}

