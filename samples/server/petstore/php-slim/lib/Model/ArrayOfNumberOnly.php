<?php
/**
 * ArrayOfNumberOnly
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ArrayOfNumberOnly
 */
class ArrayOfNumberOnly
{

    /** @var float[] $arrayNumber (optional) */
    private $arrayNumber;

    /**
     * ArrayOfNumberOnly constructor
     *
     * @param float[]|null $arrayNumber (optional)
     */
    public function __construct(
        $arrayNumber = null
    ) {
        $this->arrayNumber = $arrayNumber;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $arrayOfNumberOnly = ArrayOfNumberOnly::createFromObject([ 'ArrayNumber' => 'foobar' ]);
     *
     * @return ArrayOfNumberOnly
     */
    public static function createFromObject(array $data = null)
    {
        $arrayNumber = (isset($data['ArrayNumber'])) ? $data['ArrayNumber'] : null;
        return new ArrayOfNumberOnly(
            $arrayNumber
        );
    }
}
