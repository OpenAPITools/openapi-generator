<?php
/**
 * AnimalFarm
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * AnimalFarm
 */
class AnimalFarm
{

    /**
     * AnimalFarm constructor
     */
    public function __construct()
    {
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $animalFarm = AnimalFarm::createFromObject();
     *
     * @return AnimalFarm
     */
    public static function createFromObject(array $data = null)
    {
        return new AnimalFarm();
    }
}
