<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Query parameters for findPetsByStatus
 */
class FindPetsByStatusQueryData
{
    /**
     * Status values that need to be considered for filter
     */
    #[DTA\Data(field: "status")]
    #[DTA\Strategy("QueryStringScalarArray", ["type" => "string", "format" => "csv"])]
    #[DTA\Validator("QueryStringScalarArray", ["type" => "string", "format" => "csv"])]
    public array|null $status = null;

}
