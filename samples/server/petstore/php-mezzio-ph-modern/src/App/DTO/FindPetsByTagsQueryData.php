<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Query parameters for findPetsByTags
 */
class FindPetsByTagsQueryData
{
    /**
     * Tags to filter by
     */
    #[DTA\Data(field: "tags")]
    #[DTA\Strategy("QueryStringScalarArray", ["type" => "string", "format" => "csv"])]
    #[DTA\Validator("QueryStringScalarArray", ["type" => "string", "format" => "csv"])]
    public array|null $tags = null;

}
