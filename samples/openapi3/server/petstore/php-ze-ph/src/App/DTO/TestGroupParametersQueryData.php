<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Query parameters for testGroupParameters
 */
class TestGroupParametersQueryData
{
    /**
     * Required String in group parameters
     * @DTA\Data(field="required_string_group")
     * @DTA\Strategy(name="QueryParameter", options={"type":"int"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"int"})
     * @var int
     */
    public $required_string_group;
    /**
     * Integer in group parameters
     * @DTA\Data(field="int64_group", nullable=true)
     * @DTA\Strategy(name="QueryParameter", options={"type":"int"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"int"})
     * @var int
     */
    public $int64_group;
    /**
     * String in group parameters
     * @DTA\Data(field="string_group", nullable=true)
     * @DTA\Strategy(name="QueryParameter", options={"type":"int"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"int"})
     * @var int
     */
    public $string_group;
    /**
     * Required Integer in group parameters
     * @DTA\Data(field="required_int64_group")
     * @DTA\Strategy(name="QueryParameter", options={"type":"int"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"int"})
     * @var int
     */
    public $required_int64_group;
}
