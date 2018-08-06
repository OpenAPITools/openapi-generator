<?php
/**
 * OuterComposite
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * OuterComposite
 */
class OuterComposite
{

    /** @var float $myNumber (optional) */
    private $myNumber;

    /** @var string $myString (optional) */
    private $myString;

    /** @var bool $myBoolean (optional) */
    private $myBoolean;

    /**
     * OuterComposite constructor
     *
     * @param float|null $myNumber (optional)
     * @param string|null $myString (optional)
     * @param bool|null $myBoolean (optional)
     */
    public function __construct(
        $myNumber = null,
        $myString = null,
        $myBoolean = null
    ) {
        $this->myNumber = $myNumber;
        $this->myString = $myString;
        $this->myBoolean = $myBoolean;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $outerComposite = OuterComposite::createFromObject([ 'my_number' => 'foobar' ]);
     *
     * @return OuterComposite
     */
    public static function createFromObject(array $data = null)
    {
        $myNumber = (isset($data['my_number'])) ? $data['my_number'] : null;
        $myString = (isset($data['my_string'])) ? $data['my_string'] : null;
        $myBoolean = (isset($data['my_boolean'])) ? $data['my_boolean'] : null;
        return new OuterComposite(
            $myNumber,
            $myString,
            $myBoolean
        );
    }
}
