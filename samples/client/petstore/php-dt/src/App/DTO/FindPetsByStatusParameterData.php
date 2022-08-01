<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for findPetsByStatus
 */
class FindPetsByStatusParameterData
{
    /**
     * Status values that need to be considered for filter
     * @DTA\Data(subset="query", field="status")
     * @DTA\Strategy(subset="query", name="QueryStringScalarArray", options={"type":"string", "format":"csv"})
     * @DTA\Validator(subset="query", name="QueryStringScalarArray", options={"type":"string", "format":"csv"})
     * @var string[]|null
     */
    public $status;

}
