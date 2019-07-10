<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class InlineObject4
{
    /**
     * field1
     * @DTA\Data(field="param")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $param;
    /**
     * field2
     * @DTA\Data(field="param2")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $param2;
}
