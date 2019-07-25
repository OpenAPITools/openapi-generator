<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class TypeHolderExample
{
    /**
     * @DTA\Data(field="string_item")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $string_item;
    /**
     * @DTA\Data(field="number_item")
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $number_item;
    /**
     * @DTA\Data(field="integer_item")
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $integer_item;
    /**
     * @DTA\Data(field="bool_item")
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $bool_item;
    /**
     * @DTA\Data(field="array_item")
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $array_item;
}
