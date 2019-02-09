<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class XmlItem
{
    /**
     * @DTA\Data(field="attribute_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $attribute_string;
    /**
     * @DTA\Data(field="attribute_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $attribute_number;
    /**
     * @DTA\Data(field="attribute_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $attribute_integer;
    /**
     * @DTA\Data(field="attribute_boolean", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $attribute_boolean;
    /**
     * @DTA\Data(field="wrapped_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $wrapped_array;
    /**
     * @DTA\Data(field="name_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $name_string;
    /**
     * @DTA\Data(field="name_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $name_number;
    /**
     * @DTA\Data(field="name_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $name_integer;
    /**
     * @DTA\Data(field="name_boolean", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $name_boolean;
    /**
     * @DTA\Data(field="name_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $name_array;
    /**
     * @DTA\Data(field="name_wrapped_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $name_wrapped_array;
    /**
     * @DTA\Data(field="prefix_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $prefix_string;
    /**
     * @DTA\Data(field="prefix_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $prefix_number;
    /**
     * @DTA\Data(field="prefix_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $prefix_integer;
    /**
     * @DTA\Data(field="prefix_boolean", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $prefix_boolean;
    /**
     * @DTA\Data(field="prefix_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $prefix_array;
    /**
     * @DTA\Data(field="prefix_wrapped_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $prefix_wrapped_array;
    /**
     * @DTA\Data(field="namespace_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $namespace_string;
    /**
     * @DTA\Data(field="namespace_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $namespace_number;
    /**
     * @DTA\Data(field="namespace_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $namespace_integer;
    /**
     * @DTA\Data(field="namespace_boolean", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $namespace_boolean;
    /**
     * @DTA\Data(field="namespace_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $namespace_array;
    /**
     * @DTA\Data(field="namespace_wrapped_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $namespace_wrapped_array;
    /**
     * @DTA\Data(field="prefix_ns_string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $prefix_ns_string;
    /**
     * @DTA\Data(field="prefix_ns_number", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @var float
     */
    public $prefix_ns_number;
    /**
     * @DTA\Data(field="prefix_ns_integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $prefix_ns_integer;
    /**
     * @DTA\Data(field="prefix_ns_boolean", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"bool"})
     * @var bool
     */
    public $prefix_ns_boolean;
    /**
     * @DTA\Data(field="prefix_ns_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $prefix_ns_array;
    /**
     * @DTA\Data(field="prefix_ns_wrapped_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Type", "options":{"type":"int"}}
     * }})
     * @var int[]
     */
    public $prefix_ns_wrapped_array;
}
