<?php
/**
 * Animal
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Animal
 */
class Animal
{

    /** @var string $className */
    private $className;

    /** @var string $color (optional) Default 'red' */
    private $color = 'red';

    /**
     * Animal constructor
     *
     * @param string $className
     * @param string|null $color (optional) Default 'red'
     */
    public function __construct(
        $className,
        $color = 'red'
    ) {
        $this->className = $className;
        $this->color = $color;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $animal = Animal::createFromObject([ 'className' => 'foobar' ]);
     *
     * @return Animal
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['className'] === null) {
            throw new InvalidArgumentException("'className' can't be null");
        }
        $className = $data['className'];
        $color = (isset($data['color'])) ? $data['color'] : 'red';
        return new Animal(
            $className,
            $color
        );
    }
}
