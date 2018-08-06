<?php
/**
 * EnumTest
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * EnumTest
 */
class EnumTest
{

    /** @var string $enumStringRequired */
    private $enumStringRequired;

    /** @var string $enumString (optional) */
    private $enumString;

    /** @var int $enumInteger (optional) */
    private $enumInteger;

    /** @var double $enumNumber (optional) */
    private $enumNumber;

    /** @var \OpenAPIServer\Model\OuterEnum $outerEnum (optional) */
    private $outerEnum;

    /**
     * EnumTest constructor
     *
     * @param string $enumStringRequired
     * @param string|null $enumString (optional)
     * @param int|null $enumInteger (optional)
     * @param double|null $enumNumber (optional)
     * @param \OpenAPIServer\Model\OuterEnum|null $outerEnum (optional)
     */
    public function __construct(
        $enumStringRequired,
        $enumString = null,
        $enumInteger = null,
        $enumNumber = null,
        $outerEnum = null
    ) {
        $this->enumString = $enumString;
        $this->enumStringRequired = $enumStringRequired;
        $this->enumInteger = $enumInteger;
        $this->enumNumber = $enumNumber;
        $this->outerEnum = $outerEnum;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $enumTest = EnumTest::createFromObject([ 'enum_string' => 'foobar' ]);
     *
     * @return EnumTest
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['enum_string_required'] === null) {
            throw new InvalidArgumentException("'enum_string_required' can't be null");
        }
        $enumStringRequired = $data['enum_string_required'];
        $enumString = (isset($data['enum_string'])) ? $data['enum_string'] : null;
        $enumInteger = (isset($data['enum_integer'])) ? $data['enum_integer'] : null;
        $enumNumber = (isset($data['enum_number'])) ? $data['enum_number'] : null;
        $outerEnum = (isset($data['outerEnum'])) ? $data['outerEnum'] : null;
        return new EnumTest(
            $enumStringRequired,
            $enumString,
            $enumInteger,
            $enumNumber,
            $outerEnum
        );
    }
}
