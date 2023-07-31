<?php
/**
 * EnumTest
 */
namespace app\Models;

/**
 * EnumTest
 */
class EnumTest {

    /** @var string $enumString */
    public $enumString = "";

    /** @var string $enumStringRequired */
    public $enumStringRequired = "";

    /** @var int $enumInteger */
    public $enumInteger = 0;

    /** @var float $enumNumber */
    public $enumNumber = 0;

    /** @var string|null $outerEnum */
    public $outerEnum = null;

    /** @var int $outerEnumInteger */
    public $outerEnumInteger = \app\Models\OuterEnumInteger::NUMBER_0;

    /** @var string $outerEnumDefaultValue */
    public $outerEnumDefaultValue = \app\Models\OuterEnumDefaultValue::PLACED;

    /** @var int $outerEnumIntegerDefaultValue */
    public $outerEnumIntegerDefaultValue = \app\Models\OuterEnumIntegerDefaultValue::NUMBER_0;

}
