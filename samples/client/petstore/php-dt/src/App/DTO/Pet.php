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
     * @DTA\Data(field="id")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $id = 0;

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
     * @var string
     */
    public $name = "";

    /**
     * @DTA\Data(field="photoUrls")
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\Collection32::class})
     * @DTA\Validator(name="TypeCompliant", options={"type":\App\DTO\Collection32::class})
     * @var \App\DTO\Collection32
     */
    public $photo_urls = [];

    /**
     * @DTA\Data(field="tags")
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\Collection33::class})
     * @DTA\Validator(name="TypeCompliant", options={"type":\App\DTO\Collection33::class})
     * @var \App\DTO\Collection33
     */
    public $tags = [];

    /**
     * pet status in the store
     * @DTA\Data(field="status")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $status = "";

}
