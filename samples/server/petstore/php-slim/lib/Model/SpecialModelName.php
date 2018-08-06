<?php
/**
 * SpecialModelName
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * SpecialModelName
 */
class SpecialModelName
{

    /** @var int $specialPropertyName (optional) */
    private $specialPropertyName;

    /**
     * SpecialModelName constructor
     *
     * @param int|null $specialPropertyName (optional)
     */
    public function __construct(
        $specialPropertyName = null
    ) {
        $this->specialPropertyName = $specialPropertyName;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $specialModelName = SpecialModelName::createFromObject([ '$special[property.name]' => 'foobar' ]);
     *
     * @return SpecialModelName
     */
    public static function createFromObject(array $data = null)
    {
        $specialPropertyName = (isset($data['$special[property.name]'])) ? $data['$special[property.name]'] : null;
        return new SpecialModelName(
            $specialPropertyName
        );
    }
}
