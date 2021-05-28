<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Describes the result of uploading an image resource
 */
class ApiResponse
{
    #[DTA\Data(field: "code", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $code = null;

    #[DTA\Data(field: "type", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $type = null;

    #[DTA\Data(field: "message", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $message = null;

}
