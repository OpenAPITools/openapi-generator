<?php
/**
 * EnumArrays
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * EnumArrays
 */
class EnumArrays
{

    /** @var string $justSymbol (optional) */
    private $justSymbol;

    /** @var string[] $arrayEnum (optional) */
    private $arrayEnum;

    /**
     * EnumArrays constructor
     *
     * @param string|null $justSymbol (optional)
     * @param string[]|null $arrayEnum (optional)
     */
    public function __construct(
        $justSymbol = null,
        $arrayEnum = null
    ) {
        $this->justSymbol = $justSymbol;
        $this->arrayEnum = $arrayEnum;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $enumArrays = EnumArrays::createFromObject([ 'just_symbol' => 'foobar' ]);
     *
     * @return EnumArrays
     */
    public static function createFromObject(array $data = null)
    {
        $justSymbol = (isset($data['just_symbol'])) ? $data['just_symbol'] : null;
        $arrayEnum = (isset($data['array_enum'])) ? $data['array_enum'] : null;
        return new EnumArrays(
            $justSymbol,
            $arrayEnum
        );
    }
}
