<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for getUserByName
 */
class GetUserByNameParameterData
{
    /**
     * The name that needs to be fetched. Use user1 for testing.
     */
    #[DTA\Data(subset: "path", field: "username")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"], subset: "path")]
    public string|null $username = null;

}
