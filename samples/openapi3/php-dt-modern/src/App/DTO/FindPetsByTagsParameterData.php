<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for findPetsByTags
 */
class FindPetsByTagsParameterData
{
    /**
     * Tags to filter by
     */
    #[DTA\Data(subset: "query", field: "tags")]
    #[DTA\Strategy("QueryStringScalarArray", ["type" => "string", "format" => "csv"], "query")]
    #[DTA\Validator("QueryStringScalarArray", ["type" => "string", "format" => "csv"], subset: "query")]
    public array|null $tags = null;

}
