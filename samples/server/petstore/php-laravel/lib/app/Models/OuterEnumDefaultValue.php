<?php
/**
 * OuterEnumDefaultValue
 */
namespace app\Models;

/**
 * OuterEnumDefaultValue
 */
class OuterEnumDefaultValue
{
    /**
     * Possible values of this enum
     */
    const PLACED = 'placed';

    const APPROVED = 'approved';

    const DELIVERED = 'delivered';

    /**
     * Gets allowable values of the enum
     * @return string[]
     */
    public static function getAllowableEnumValues()
    {
        return [
            self::PLACED,
            self::APPROVED,
            self::DELIVERED
        ];
    }
}
