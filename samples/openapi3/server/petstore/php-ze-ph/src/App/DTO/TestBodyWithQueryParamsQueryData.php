<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Query parameters for testBodyWithQueryParams
 */
class TestBodyWithQueryParamsQueryData
{
    /**
     * @DTA\Data(field="query")
     * @DTA\Strategy(name="QueryParameter", options={"type":"string"})
     * @DTA\Validator(name="QueryParameterType", options={"type":"string"})
     * @var string
     */
    public $query;
}
