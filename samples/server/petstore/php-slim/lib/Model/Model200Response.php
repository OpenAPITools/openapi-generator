<?php
/**
 * Model200Response
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Model200Response
 */
class Model200Response
{

    /** @var int $name (optional) */
    private $name;

    /** @var string $class (optional) */
    private $class;

    /**
     * Model200Response constructor
     *
     * @param int|null $name (optional)
     * @param string|null $class (optional)
     */
    public function __construct(
        $name = null,
        $class = null
    ) {
        $this->name = $name;
        $this->class = $class;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $_200response = Model200Response::createFromObject([ 'name' => 'foobar' ]);
     *
     * @return Model200Response
     */
    public static function createFromObject(array $data = null)
    {
        $name = (isset($data['name'])) ? $data['name'] : null;
        $class = (isset($data['class'])) ? $data['class'] : null;
        return new Model200Response(
            $name,
            $class
        );
    }
}
