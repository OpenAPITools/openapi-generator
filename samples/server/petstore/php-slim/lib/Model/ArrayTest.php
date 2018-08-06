<?php
/**
 * ArrayTest
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ArrayTest
 */
class ArrayTest
{

    /** @var string[] $arrayOfString (optional) */
    private $arrayOfString;

    /** @var int[][] $arrayArrayOfInteger (optional) */
    private $arrayArrayOfInteger;

    /** @var \OpenAPIServer\Model\ReadOnlyFirst[][] $arrayArrayOfModel (optional) */
    private $arrayArrayOfModel;

    /**
     * ArrayTest constructor
     *
     * @param string[]|null $arrayOfString (optional)
     * @param int[][]|null $arrayArrayOfInteger (optional)
     * @param \OpenAPIServer\Model\ReadOnlyFirst[][]|null $arrayArrayOfModel (optional)
     */
    public function __construct(
        $arrayOfString = null,
        $arrayArrayOfInteger = null,
        $arrayArrayOfModel = null
    ) {
        $this->arrayOfString = $arrayOfString;
        $this->arrayArrayOfInteger = $arrayArrayOfInteger;
        $this->arrayArrayOfModel = $arrayArrayOfModel;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $arrayTest = ArrayTest::createFromObject([ 'array_of_string' => 'foobar' ]);
     *
     * @return ArrayTest
     */
    public static function createFromObject(array $data = null)
    {
        $arrayOfString = (isset($data['array_of_string'])) ? $data['array_of_string'] : null;
        $arrayArrayOfInteger = (isset($data['array_array_of_integer'])) ? $data['array_array_of_integer'] : null;
        $arrayArrayOfModel = (isset($data['array_array_of_model'])) ? $data['array_array_of_model'] : null;
        return new ArrayTest(
            $arrayOfString,
            $arrayArrayOfInteger,
            $arrayArrayOfModel
        );
    }
}
