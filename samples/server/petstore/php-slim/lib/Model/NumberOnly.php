<?php
/**
 * NumberOnly
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * NumberOnly
 */
class NumberOnly
{

    /** @var float $justNumber (optional) */
    private $justNumber;

    /**
     * NumberOnly constructor
     *
     * @param float|null $justNumber (optional)
     */
    public function __construct(
        $justNumber = null
    ) {
        $this->justNumber = $justNumber;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $numberOnly = NumberOnly::createFromObject([ 'JustNumber' => 'foobar' ]);
     *
     * @return NumberOnly
     */
    public static function createFromObject(array $data = null)
    {
        $justNumber = (isset($data['JustNumber'])) ? $data['JustNumber'] : null;
        return new NumberOnly(
            $justNumber
        );
    }
}
