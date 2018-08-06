<?php
/**
 * MapTest
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * MapTest
 */
class MapTest
{

    /** @var map[string,map[string,string]] $mapMapOfString (optional) */
    private $mapMapOfString;

    /** @var map[string,string] $mapOfEnumString (optional) */
    private $mapOfEnumString;

    /** @var map[string,bool] $directMap (optional) */
    private $directMap;

    /** @var \OpenAPIServer\Model\StringBooleanMap $indirectMap (optional) */
    private $indirectMap;

    /**
     * MapTest constructor
     *
     * @param map[string,map[string,string]]|null $mapMapOfString (optional)
     * @param map[string,string]|null $mapOfEnumString (optional)
     * @param map[string,bool]|null $directMap (optional)
     * @param \OpenAPIServer\Model\StringBooleanMap|null $indirectMap (optional)
     */
    public function __construct(
        $mapMapOfString = null,
        $mapOfEnumString = null,
        $directMap = null,
        $indirectMap = null
    ) {
        $this->mapMapOfString = $mapMapOfString;
        $this->mapOfEnumString = $mapOfEnumString;
        $this->directMap = $directMap;
        $this->indirectMap = $indirectMap;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $mapTest = MapTest::createFromObject([ 'map_map_of_string' => 'foobar' ]);
     *
     * @return MapTest
     */
    public static function createFromObject(array $data = null)
    {
        $mapMapOfString = (isset($data['map_map_of_string'])) ? $data['map_map_of_string'] : null;
        $mapOfEnumString = (isset($data['map_of_enum_string'])) ? $data['map_of_enum_string'] : null;
        $directMap = (isset($data['direct_map'])) ? $data['direct_map'] : null;
        $indirectMap = (isset($data['indirect_map'])) ? $data['indirect_map'] : null;
        return new MapTest(
            $mapMapOfString,
            $mapOfEnumString,
            $directMap,
            $indirectMap
        );
    }
}
