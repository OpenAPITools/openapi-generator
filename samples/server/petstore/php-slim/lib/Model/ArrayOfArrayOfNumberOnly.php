<?php
/**
 * ArrayOfArrayOfNumberOnly
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ArrayOfArrayOfNumberOnly
 */
class ArrayOfArrayOfNumberOnly
{

    /** @var float[][] $arrayArrayNumber (optional) */
    private $arrayArrayNumber;

    /**
     * ArrayOfArrayOfNumberOnly constructor
     *
     * @param float[][]|null $arrayArrayNumber (optional)
     */
    public function __construct(
        $arrayArrayNumber = null
    ) {
        $this->arrayArrayNumber = $arrayArrayNumber;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $arrayOfArrayOfNumberOnly = ArrayOfArrayOfNumberOnly::createFromObject([ 'ArrayArrayNumber' => 'foobar' ]);
     *
     * @return ArrayOfArrayOfNumberOnly
     */
    public static function createFromObject(array $data = null)
    {
        $arrayArrayNumber = (isset($data['ArrayArrayNumber'])) ? $data['ArrayArrayNumber'] : null;
        return new ArrayOfArrayOfNumberOnly(
            $arrayArrayNumber
        );
    }
}
