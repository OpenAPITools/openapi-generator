<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Model for testing model name starting with number
 */
class Model200Response 
{
    /**
     * @DTA\Data(field="name", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $name;
    /**
     * @DTA\Data(field="class", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $class;
}

