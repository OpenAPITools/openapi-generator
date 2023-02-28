<?php
/**
 * EnumClass
 */
namespace app\Models;

/**
 * EnumClass
 */
class EnumClass
{
    /**
     * Possible values of this enum
     */
    const ABC = '_abc';

    const EFG = '-efg';

    const XYZ = '(xyz)';

    /**
     * Gets allowable values of the enum
     * @return string[]
     */
    public static function getAllowableEnumValues()
    {
        return [
            self::ABC,
            self::EFG,
            self::XYZ
        ];
    }
}
