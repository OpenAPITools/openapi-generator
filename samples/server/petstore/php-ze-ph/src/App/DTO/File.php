<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Must be named &#x60;File&#x60; for test.
 */
class File
{
    /**
     * Test capitalization
     * @DTA\Data(field="sourceURI", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $source_uri;
}
