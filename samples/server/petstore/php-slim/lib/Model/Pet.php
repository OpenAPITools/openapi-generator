<?php
/**
 * Pet
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Pet
 */
class Pet
{

    /** @var string $name */
    private $name;

    /** @var string[] $photoUrls */
    private $photoUrls;

    /** @var int $id (optional) */
    private $id;

    /** @var \OpenAPIServer\Model\Category $category (optional) */
    private $category;

    /** @var \OpenAPIServer\Model\Tag[] $tags (optional) */
    private $tags;

    /** @var string $status (optional) pet status in the store */
    private $status;

    /**
     * Pet constructor
     *
     * @param string $name
     * @param string[] $photoUrls
     * @param int|null $id (optional)
     * @param \OpenAPIServer\Model\Category|null $category (optional)
     * @param \OpenAPIServer\Model\Tag[]|null $tags (optional)
     * @param string|null $status (optional) pet status in the store
     */
    public function __construct(
        $name,
        $photoUrls,
        $id = null,
        $category = null,
        $tags = null,
        $status = null
    ) {
        $this->id = $id;
        $this->category = $category;
        $this->name = $name;
        $this->photoUrls = $photoUrls;
        $this->tags = $tags;
        $this->status = $status;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $pet = Pet::createFromObject([ 'id' => 'foobar' ]);
     *
     * @return Pet
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['name'] === null) {
            throw new InvalidArgumentException("'name' can't be null");
        }
        $name = $data['name'];
        if ($data['photoUrls'] === null) {
            throw new InvalidArgumentException("'photoUrls' can't be null");
        }
        $photoUrls = $data['photoUrls'];
        $id = (isset($data['id'])) ? $data['id'] : null;
        $category = (isset($data['category'])) ? $data['category'] : null;
        $tags = (isset($data['tags'])) ? $data['tags'] : null;
        $status = (isset($data['status'])) ? $data['status'] : null;
        return new Pet(
            $name,
            $photoUrls,
            $id,
            $category,
            $tags,
            $status
        );
    }
}
