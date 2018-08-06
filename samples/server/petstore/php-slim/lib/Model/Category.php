<?php
/**
 * Category
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Category
 */
class Category
{

    /** @var int $id (optional) */
    private $id;

    /** @var string $name (optional) */
    private $name;

    /**
     * Category constructor
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
     * @example $category = Category::createFromObject([ 'id' => 'foobar' ]);
     *
     * @return Category
     */
    public static function createFromObject(array $data = null)
    {
        $id = (isset($data['id'])) ? $data['id'] : null;
        $name = (isset($data['name'])) ? $data['name'] : null;
        return new Category(
            $id,
            $name
        );
    }
}
