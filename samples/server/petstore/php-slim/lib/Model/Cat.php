<?php
/**
 * Cat
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Cat
 */
class Cat
{

    /** @var string $className */
    private $className;

    /** @var string $color (optional) Default 'red' */
    private $color = 'red';

    /** @var bool $declawed (optional) */
    private $declawed;

    /**
     * Cat constructor
     *
     * @param string $className
     * @param string|null $color (optional) Default 'red'
     * @param bool|null $declawed (optional)
     */
    public function __construct(
        $className,
        $color = 'red',
        $declawed = null
    ) {
        $this->className = $className;
        $this->color = $color;
        $this->declawed = $declawed;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $cat = Cat::createFromObject([ 'className' => 'foobar' ]);
     *
     * @return Cat
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['className'] === null) {
            throw new InvalidArgumentException("'className' can't be null");
        }
        $className = $data['className'];
        $color = (isset($data['color'])) ? $data['color'] : 'red';
        $declawed = (isset($data['declawed'])) ? $data['declawed'] : null;
        return new Cat(
            $className,
            $color,
            $declawed
        );
    }
}
