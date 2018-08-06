<?php
/**
 * OuterEnum
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * OuterEnum
 */
class OuterEnum
{

    /**
     * OuterEnum constructor
     */
    public function __construct()
    {
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $outerEnum = OuterEnum::createFromObject();
     *
     * @return OuterEnum
     */
    public static function createFromObject(array $data = null)
    {
        return new OuterEnum();
    }
}
