<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class ArrayOfNumberOnly
{
    /**
     * @DTA\Data(field="ArrayNumber", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"float"}}
     * }})
     * @var float[]
     */
    public $array_number;
}
