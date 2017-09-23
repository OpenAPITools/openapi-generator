<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class NumberOnly
{
    /**
     * @DTA\Data(field="JustNumber", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $just_number;
}
