<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for findPetsByTags
 */
class FindPetsByTagsParameterData
{
    /**
     * Tags to filter by
     * @DTA\Data(subset="query", field="tags")
     * @DTA\Strategy(subset="query", name="QueryStringScalarArray", options={"type":\App\DTO\Collection5::class, "format":"csv"})
     * @DTA\Validator(subset="query", name="QueryStringScalarArray", options={"type":\App\DTO\Collection5::class, "format":"csv"})
     */
    public ?array $tags = null;

}
