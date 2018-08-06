<?php
/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */
class MixedPropertiesAndAdditionalPropertiesClass
{

    /** @var string $uuid (optional) */
    private $uuid;

    /** @var \DateTime $dateTime (optional) */
    private $dateTime;

    /** @var map[string,\OpenAPIServer\Model\Animal] $map (optional) */
    private $map;

    /**
     * MixedPropertiesAndAdditionalPropertiesClass constructor
     *
     * @param string|null $uuid (optional)
     * @param \DateTime|null $dateTime (optional)
     * @param map[string,\OpenAPIServer\Model\Animal]|null $map (optional)
     */
    public function __construct(
        $uuid = null,
        $dateTime = null,
        $map = null
    ) {
        $this->uuid = $uuid;
        $this->dateTime = $dateTime;
        $this->map = $map;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $mixedPropertiesAndAdditionalPropertiesClass = MixedPropertiesAndAdditionalPropertiesClass::createFromObject([ 'uuid' => 'foobar' ]);
     *
     * @return MixedPropertiesAndAdditionalPropertiesClass
     */
    public static function createFromObject(array $data = null)
    {
        $uuid = (isset($data['uuid'])) ? $data['uuid'] : null;
        $dateTime = (isset($data['dateTime'])) ? $data['dateTime'] : null;
        $map = (isset($data['map'])) ? $data['map'] : null;
        return new MixedPropertiesAndAdditionalPropertiesClass(
            $uuid,
            $dateTime,
            $map
        );
    }
}
