<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Model for testing reserved words
 */
class ModelReturn
{
    /**
     * @DTA\Data(field="return", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $return;
}
