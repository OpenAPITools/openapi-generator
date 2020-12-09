<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * A pet for sale in the pet store
 */
class Pet
{
    /**
     * @DTA\Data(field="id", nullable=true)
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int|null
     */
    public $id;
    /**
     * @DTA\Data(field="category", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\Category::class})
     * @DTA\Validator(name="TypeCompliant", options={"type":\App\DTO\Category::class})
     * @var \App\DTO\Category|null
     */
    public $category;
    /**
     * @DTA\Data(field="name")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string|null
     */
    public $name;
    /**
     * @DTA\Data(field="photoUrls")
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Scalar", "options":{"type":"string"}}
     * }})
     * @var string[]|null
     */
    public $photo_urls;
    /**
     * @DTA\Data(field="tags", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":\App\DTO\Tag::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"TypeCompliant", "options":{"type":\App\DTO\Tag::class}}
     * }})
     * @var \App\DTO\Tag[]|null
     */
    public $tags;
    /**
     * pet status in the store
     * @DTA\Data(field="status", nullable=true)
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string|null
     */
    public $status;
}
