<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class InlineObject2
{
    /**
     * Form parameter enum test (string array)
     * @DTA\Data(field="enum_form_string_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"string"}}
     * }})
     * @var string[]
     */
    public $enum_form_string_array;
    /**
     * Form parameter enum test (string)
     * @DTA\Data(field="enum_form_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $enum_form_string;
}
