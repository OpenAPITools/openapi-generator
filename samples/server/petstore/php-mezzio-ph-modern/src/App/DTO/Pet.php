<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * A pet for sale in the pet store
 */
class Pet
{
    #[DTA\Data(field: "id", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $id = null;

    #[DTA\Data(field: "category", nullable: true)]
    #[DTA\Strategy("Object", ["type" => \App\DTO\Category::class])]
    #[DTA\Validator("TypeCompliant", ["type" => \App\DTO\Category::class])]
    public \App\DTO\Category|null $category = null;

    #[DTA\Data(field: "name")]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $name = null;

    #[DTA\Data(field: "photoUrls")]
    #[DTA\Strategy("Object", ["type" => \App\DTO\Collection32::class])]
    #[DTA\Validator("TypeCompliant", ["type" => \App\DTO\Collection32::class])]
    public \App\DTO\Collection32|null $photo_urls = null;

    #[DTA\Data(field: "tags", nullable: true)]
    #[DTA\Strategy("Object", ["type" => \App\DTO\Collection33::class])]
    #[DTA\Validator("TypeCompliant", ["type" => \App\DTO\Collection33::class])]
    public \App\DTO\Collection33|null $tags = null;

    /**
     * pet status in the store
     */
    #[DTA\Data(field: "status", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $status = null;

}
