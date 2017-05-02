<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class Dog 
{
    /**
     * @DTA\Data(field="className")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $class_name;
    /**
     * @DTA\Data(field="color", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $color;
    /**
     * @DTA\Data(field="breed", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $breed;
}

