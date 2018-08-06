<?php
/**
 * EnumClass
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * EnumClass
 */
class EnumClass
{

    /**
     * EnumClass constructor
     */
    public function __construct()
    {
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $enumClass = EnumClass::createFromObject();
     *
     * @return EnumClass
     */
    public static function createFromObject(array $data = null)
    {
        return new EnumClass();
    }
}
