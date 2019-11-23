<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class NullableClass
{
    /**
     * @DTA\Data(field="integer_prop", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $integer_prop;
    /**
     * @DTA\Data(field="number_prop", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $number_prop;
    /**
     * @DTA\Data(field="boolean_prop", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $boolean_prop;
    /**
     * @DTA\Data(field="string_prop", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $string_prop;
    /**
     * @DTA\Data(field="date_prop", nullable=true)
     * @DTA\Strategy(name="Date")
     * @DTA\Validator(name="Date")
     * @var \DateTime
     */
    public $date_prop;
    /**
     * @DTA\Data(field="datetime_prop", nullable=true)
     * @DTA\Strategy(name="DateTime")
     * @DTA\Validator(name="Date", options={"format": \DateTime::RFC3339})
     * @var \DateTime
     */
    public $datetime_prop;
    /**
     * @DTA\Data(field="array_nullable_prop", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"object"}}
     * }})
     * @var object[]
     */
    public $array_nullable_prop;
    /**
     * @DTA\Data(field="array_and_items_nullable_prop", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"object"}}
     * }})
     * @var object[]
     */
    public $array_and_items_nullable_prop;
    /**
     * @DTA\Data(field="array_items_nullable", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"object"}}
     * }})
     * @var object[]
     */
    public $array_items_nullable;
    /**
     * @DTA\Data(field="object_nullable_prop", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"object"}}
     * }})
     * @var map[string,object]
     */
    public $object_nullable_prop;
    /**
     * @DTA\Data(field="object_and_items_nullable_prop", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"object"}}
     * }})
     * @var map[string,object]
     */
    public $object_and_items_nullable_prop;
    /**
     * @DTA\Data(field="object_items_nullable", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"object"}}
     * }})
     * @var map[string,object]
     */
    public $object_items_nullable;
}
