<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * A User who is purchasing from the pet store
 */
class User
{
    #[DTA\Data(field: "id", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $id = null;

    #[DTA\Data(field: "username", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $username = null;

    #[DTA\Data(field: "firstName", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $first_name = null;

    #[DTA\Data(field: "lastName", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $last_name = null;

    #[DTA\Data(field: "email", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $email = null;

    #[DTA\Data(field: "password", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $password = null;

    #[DTA\Data(field: "phone", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $phone = null;

    /**
     * User Status
     */
    #[DTA\Data(field: "userStatus", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $user_status = null;

}
