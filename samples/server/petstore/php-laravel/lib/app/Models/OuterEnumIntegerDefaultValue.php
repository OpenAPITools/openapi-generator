<?php
/**
 * OuterEnumIntegerDefaultValue
 */
namespace app\Models;

/**
 * OuterEnumIntegerDefaultValue
 */
class OuterEnumIntegerDefaultValue
{
    /**
     * Possible values of this enum
     */
    const NUMBER_0 = 0;

    const NUMBER_1 = 1;

    const NUMBER_2 = 2;

    /**
     * Gets allowable values of the enum
     * @return string[]
     */
    public static function getAllowableEnumValues()
    {
        return [
            self::NUMBER_0,
            self::NUMBER_1,
            self::NUMBER_2
        ];
    }
}
