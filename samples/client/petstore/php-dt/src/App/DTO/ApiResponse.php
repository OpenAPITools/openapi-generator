<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Describes the result of uploading an image resource
 */
class ApiResponse
{
    /**
     * @DTA\Data(field="code")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $code = 0;

    /**
     * @DTA\Data(field="type")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $type = "";

    /**
     * @DTA\Data(field="message")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $message = "";

}
