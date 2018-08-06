<?php
/**
 * Tag
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Tag
 */
class Tag
{

    /** @var int $id (optional) */
    private $id;

    /** @var string $name (optional) */
    private $name;

    /**
     * Tag constructor
     *
     * @param int|null $id (optional)
     * @param string|null $name (optional)
     */
    public function __construct(
        $id = null,
        $name = null
    ) {
        $this->id = $id;
        $this->name = $name;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $tag = Tag::createFromObject([ 'id' => 'foobar' ]);
     *
     * @return Tag
     */
    public static function createFromObject(array $data = null)
    {
        $id = (isset($data['id'])) ? $data['id'] : null;
        $name = (isset($data['name'])) ? $data['name'] : null;
        return new Tag(
            $id,
            $name
        );
    }
}
