<?php
/**
 * StringBooleanMap
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * StringBooleanMap
 */
class StringBooleanMap
{

    /**
     * StringBooleanMap constructor
     */
    public function __construct()
    {
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $stringBooleanMap = StringBooleanMap::createFromObject();
     *
     * @return StringBooleanMap
     */
    public static function createFromObject(array $data = null)
    {
        return new StringBooleanMap();
    }
}
