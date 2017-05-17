<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Model for testing model name same as property name
 */
class Name 
{
    /**
     * @DTA\Data(field="name")
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $name;
    /**
     * @DTA\Data(field="snake_case", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $snake_case;
    /**
     * @DTA\Data(field="property", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $property;
    /**
     * @DTA\Data(field="123Number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $_123_number;
}

