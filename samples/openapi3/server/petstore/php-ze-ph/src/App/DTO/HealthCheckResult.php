<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 */
class HealthCheckResult
{
    /**
     * @DTA\Data(field="NullableMessage", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $nullable_message;
}
