<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Model for testing model with \&quot;_class\&quot; property
 */
class ClassModel
{
    /**
     * @DTA\Data(field="_class", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $_class;
}
