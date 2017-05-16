<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class SpecialModelName 
{
    /**
     * @DTA\Data(field="$special[property.name]", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $special_property_name;
}

