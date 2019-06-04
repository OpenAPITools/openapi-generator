<?php
declare(strict_types=1);

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
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterEnum::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterEnum::class})
     * @var \App\DTO\OuterEnum
     */
    public $outer_enum;
    /**
     * @DTA\Data(field="outerEnumInteger", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterEnumInteger::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterEnumInteger::class})
     * @var \App\DTO\OuterEnumInteger
     */
    public $outer_enum_integer;
    /**
     * @DTA\Data(field="outerEnumDefaultValue", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterEnumDefaultValue::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterEnumDefaultValue::class})
     * @var \App\DTO\OuterEnumDefaultValue
     */
    public $outer_enum_default_value;
    /**
     * @DTA\Data(field="outerEnumIntegerDefaultValue", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterEnumIntegerDefaultValue::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterEnumIntegerDefaultValue::class})
     * @var \App\DTO\OuterEnumIntegerDefaultValue
     */
    public $outer_enum_integer_default_value;
}
