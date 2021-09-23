<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

class InlineObject1
{
    /**
     * Additional data to pass to server
     */
    #[DTA\Data(field: "additionalMetadata", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $additional_metadata = null;

    /**
     * file to upload
     */
    #[DTA\Data(field: "file", nullable: true)]
    #[DTA\Strategy("Object", ["type" => \SplFileObject::class])]
    #[DTA\Validator("TypeCompliant", ["type" => \SplFileObject::class])]
    public \SplFileObject|null $file = null;

}
