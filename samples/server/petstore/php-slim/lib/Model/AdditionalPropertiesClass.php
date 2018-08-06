<?php
/**
 * AdditionalPropertiesClass
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * AdditionalPropertiesClass
 */
class AdditionalPropertiesClass
{

    /** @var map[string,string] $mapProperty (optional) */
    private $mapProperty;

    /** @var map[string,map[string,string]] $mapOfMapProperty (optional) */
    private $mapOfMapProperty;

    /**
     * AdditionalPropertiesClass constructor
     *
     * @param map[string,string]|null $mapProperty (optional)
     * @param map[string,map[string,string]]|null $mapOfMapProperty (optional)
     */
    public function __construct(
        $mapProperty = null,
        $mapOfMapProperty = null
    ) {
        $this->mapProperty = $mapProperty;
        $this->mapOfMapProperty = $mapOfMapProperty;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $additionalPropertiesClass = AdditionalPropertiesClass::createFromObject([ 'map_property' => 'foobar' ]);
     *
     * @return AdditionalPropertiesClass
     */
    public static function createFromObject(array $data = null)
    {
        $mapProperty = (isset($data['map_property'])) ? $data['map_property'] : null;
        $mapOfMapProperty = (isset($data['map_of_map_property'])) ? $data['map_of_map_property'] : null;
        return new AdditionalPropertiesClass(
            $mapProperty,
            $mapOfMapProperty
        );
    }
}
