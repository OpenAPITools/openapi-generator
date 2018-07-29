<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class OuterComposite
{
    /**
     * @DTA\Data(field="my_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $my_number;
    /**
     * @DTA\Data(field="my_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $my_string;
    /**
     * @DTA\Data(field="my_boolean", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $my_boolean;
}
