<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class InlineObject5
{
    /**
     * Additional data to pass to server
     * @DTA\Data(field="additionalMetadata", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $additional_metadata;
    /**
     * file to upload
     * @DTA\Data(field="requiredFile")
     * @DTA\Strategy(name="Object", options={"type":\SplFileObject::class})
     * @DTA\Validator(name="Dictionary", options={"type":\SplFileObject::class})
     * @var \SplFileObject
     */
    public $required_file;
}
