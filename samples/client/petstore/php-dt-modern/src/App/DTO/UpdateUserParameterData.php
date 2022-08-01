<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for updateUser
 */
class UpdateUserParameterData
{
    /**
     * name that need to be deleted
     */
    #[DTA\Data(subset: "path", field: "username")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"], subset: "path")]
    public string|null $username = null;

}
