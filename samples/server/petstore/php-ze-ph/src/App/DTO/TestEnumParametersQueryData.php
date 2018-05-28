<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Query parameters for testEnumParameters
 */
class TestEnumParametersQueryData
{
    /**
     * Query parameter enum test (string)
     * @DTA\Data(field="enum_query_string", nullable=true)
     * @DTA\Strategy(name="QueryParameter", options={"type":"string"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"string"})
     * @var string
     */
    public $enum_query_string;
    /**
     * Query parameter enum test (double)
     * @DTA\Data(field="enum_query_integer", nullable=true)
     * @DTA\Strategy(name="QueryParameter", options={"type":"int"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"int"})
     * @var int
     */
    public $enum_query_integer;
    /**
     * Query parameter enum test (string array)
     * @DTA\Data(field="enum_query_string_array", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="QueryParameterArray", options={"type":"string", "format":"csv"})
     * @DTA\Validator(name="QueryParameterArrayType", options={"type":"string", "format":"csv"})
     * @var string[]
     */
    public $enum_query_string_array;
}
