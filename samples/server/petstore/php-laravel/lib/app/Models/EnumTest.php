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
    private $enumString = "";

    /** @var string $enumStringRequired */
    private $enumStringRequired = "";

    /** @var int $enumInteger */
    private $enumInteger = 0;

    /** @var double $enumNumber */
    private $enumNumber = 0;

    /** @var string|null $outerEnum */
    private $outerEnum = null;

    /** @var int $outerEnumInteger */
    private $outerEnumInteger = \app\Models\OuterEnumInteger::NUMBER_0;

    /** @var string $outerEnumDefaultValue */
    private $outerEnumDefaultValue = \app\Models\OuterEnumDefaultValue::PLACED;

    /** @var int $outerEnumIntegerDefaultValue */
    private $outerEnumIntegerDefaultValue = \app\Models\OuterEnumIntegerDefaultValue::NUMBER_0;

}
