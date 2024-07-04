<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

class UploadFileRequest
{
    /**
     * Additional data to pass to server
     * @DTA\Data(field="additionalMetadata", nullable=true)
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     */
    public ?string $additional_metadata = null;

    /**
     * file to upload
     * @DTA\Data(field="file", nullable=true)
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     */
    public ?string $file = null;

}
