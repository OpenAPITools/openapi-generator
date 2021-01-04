<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Query parameters for findPetsByStatus
 */
class FindPetsByStatusQueryData
{
    /**
     * Status values that need to be considered for filter
     * @DTA\Data(field="status")
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="QueryStringScalarArray", options={"type":"string", "format":"csv"})
     * @DTA\Validator(name="QueryStringScalarArray", options={"type":"string", "format":"csv"})
     * @var string[]|null
     */
    public $status;
}
