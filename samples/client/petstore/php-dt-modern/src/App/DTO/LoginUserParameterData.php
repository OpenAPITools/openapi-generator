<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for loginUser
 */
class LoginUserParameterData
{
    /**
     * The password for login in clear text
     */
    #[DTA\Data(subset: "query", field: "password")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"], "query")]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"], subset: "query")]
    public string|null $password = null;

    /**
     * The user name for login
     */
    #[DTA\Data(subset: "query", field: "username")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"], "query")]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"], subset: "query")]
    #[DTA\Validator("Regex", ["pattern" => "/^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$/"], subset: "query")]
    public string|null $username = null;

}
