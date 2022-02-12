<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * A User who is purchasing from the pet store
 */
class User
{
    /**
     * @DTA\Data(field="id")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $id = 0;

    /**
     * @DTA\Data(field="username")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $username = "";

    /**
     * @DTA\Data(field="firstName")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $first_name = "";

    /**
     * @DTA\Data(field="lastName")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $last_name = "";

    /**
     * @DTA\Data(field="email")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $email = "";

    /**
     * @DTA\Data(field="password")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $password = "";

    /**
     * @DTA\Data(field="phone")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $phone = "";

    /**
     * User Status
     * @DTA\Data(field="userStatus")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $user_status = 0;

}
