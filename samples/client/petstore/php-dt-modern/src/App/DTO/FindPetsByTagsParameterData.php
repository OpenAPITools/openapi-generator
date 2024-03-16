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
    #[DTA\Strategy("QueryStringScalarArray", ["type" => \App\DTO\Collection5::class, "format" => "csv"], "query")]
    #[DTA\Validator("QueryStringScalarArray", ["type" => \App\DTO\Collection5::class, "format" => "csv"], subset: "query")]
    public array|null $tags = null;

}
