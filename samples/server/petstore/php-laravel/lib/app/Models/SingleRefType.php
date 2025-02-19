<?php
/**
 * SingleRefType
 */
namespace app\Models;

/**
 * SingleRefType
 */
class SingleRefType
{
    /**
     * Possible values of this enum
     */
    const ADMIN = 'admin';

    const USER = 'user';

    /**
     * Gets allowable values of the enum
     * @return string[]
     */
    public static function getAllowableEnumValues()
    {
        return [
            self::ADMIN,
            self::USER
        ];
    }
}
