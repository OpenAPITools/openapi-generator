<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class InlineObject1
{
    /**
     * Additional data to pass to server
     * @DTA\Data(field="additionalMetadata", nullable=true)
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string|null
     */
    public $additional_metadata;

    /**
     * file to upload
     * @DTA\Data(field="file", nullable=true)
     * @DTA\Validator(name="Scalar", options={"type":"\SplFileObject"})
     * @var \SplFileObject|null
     */
    public $file;

}
