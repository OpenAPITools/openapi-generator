<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class OuterComposite 
{
    /**
     * @DTA\Data(field="my_number", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterNumber::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterNumber::class})
     * @var \App\DTO\OuterNumber
     */
    public $my_number;
    /**
     * @DTA\Data(field="my_string", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterString::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterString::class})
     * @var \App\DTO\OuterString
     */
    public $my_string;
    /**
     * @DTA\Data(field="my_boolean", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\OuterBoolean::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\OuterBoolean::class})
     * @var \App\DTO\OuterBoolean
     */
    public $my_boolean;
}

