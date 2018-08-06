<?php
/**
 * Name
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Name
 */
class Name
{

    /** @var int $name */
    private $name;

    /** @var int $snakeCase (optional) */
    private $snakeCase;

    /** @var string $property (optional) */
    private $property;

    /** @var int $_123number (optional) */
    private $_123number;

    /**
     * Name constructor
     *
     * @param int $name
     * @param int|null $snakeCase (optional)
     * @param string|null $property (optional)
     * @param int|null $_123number (optional)
     */
    public function __construct(
        $name,
        $snakeCase = null,
        $property = null,
        $_123number = null
    ) {
        $this->name = $name;
        $this->snakeCase = $snakeCase;
        $this->property = $property;
        $this->_123number = $_123number;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $name = Name::createFromObject([ 'name' => 'foobar' ]);
     *
     * @return Name
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['name'] === null) {
            throw new InvalidArgumentException("'name' can't be null");
        }
        $name = $data['name'];
        $snakeCase = (isset($data['snake_case'])) ? $data['snake_case'] : null;
        $property = (isset($data['property'])) ? $data['property'] : null;
        $_123number = (isset($data['123Number'])) ? $data['123Number'] : null;
        return new Name(
            $name,
            $snakeCase,
            $property,
            $_123number
        );
    }
}
