<?php
/**
 * Dog
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Dog
 */
class Dog
{

    /** @var string $className */
    private $className;

    /** @var string $color (optional) Default 'red' */
    private $color = 'red';

    /** @var string $breed (optional) */
    private $breed;

    /**
     * Dog constructor
     *
     * @param string $className
     * @param string|null $color (optional) Default 'red'
     * @param string|null $breed (optional)
     */
    public function __construct(
        $className,
        $color = 'red',
        $breed = null
    ) {
        $this->className = $className;
        $this->color = $color;
        $this->breed = $breed;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $dog = Dog::createFromObject([ 'className' => 'foobar' ]);
     *
     * @return Dog
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['className'] === null) {
            throw new InvalidArgumentException("'className' can't be null");
        }
        $className = $data['className'];
        $color = (isset($data['color'])) ? $data['color'] : 'red';
        $breed = (isset($data['breed'])) ? $data['breed'] : null;
        return new Dog(
            $className,
            $color,
            $breed
        );
    }
}
