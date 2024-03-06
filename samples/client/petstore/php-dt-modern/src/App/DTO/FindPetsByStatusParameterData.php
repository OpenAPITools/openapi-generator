<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for findPetsByStatus
 */
class FindPetsByStatusParameterData
{
    /**
     * Status values that need to be considered for filter
     */
    #[DTA\Data(subset: "query", field: "status")]
    #[DTA\Strategy("QueryStringScalarArray", ["type" => \App\DTO\Collection2::class, "format" => "csv"], "query")]
    #[DTA\Validator("QueryStringScalarArray", ["type" => \App\DTO\Collection2::class, "format" => "csv"], subset: "query")]
    public array|null $status = null;

}
