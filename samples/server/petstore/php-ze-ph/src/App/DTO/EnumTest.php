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
     * @DTA\Data(field="enum_string_required")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $enum_string_required;
    /**
     * @DTA\Data(field="enum_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $enum_integer;
    /**
     * @DTA\Data(field="enum_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $enum_number;
    /**
     * @DTA\Data(field="outerEnum", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $outer_enum;
}
