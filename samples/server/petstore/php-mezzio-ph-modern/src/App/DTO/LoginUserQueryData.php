<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Query parameters for loginUser
 */
class LoginUserQueryData
{
    /**
     * The password for login in clear text
     */
    #[DTA\Data(field: "password")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"])]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"])]
    public string|null $password = null;

    /**
     * The user name for login
     */
    #[DTA\Data(field: "username")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"])]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"])]
    #[DTA\Validator("Regex", ["pattern" => "/^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$/"])]
    public string|null $username = null;

}
