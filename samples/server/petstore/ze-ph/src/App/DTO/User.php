<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class User
{
    /**
     * @DTA\Data(field="id", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $id;
    /**
     * @DTA\Data(field="username", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $username;
    /**
     * @DTA\Data(field="firstName", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $first_name;
    /**
     * @DTA\Data(field="lastName", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $last_name;
    /**
     * @DTA\Data(field="email", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $email;
    /**
     * @DTA\Data(field="password", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $password;
    /**
     * @DTA\Data(field="phone", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $phone;
    /**
     * User Status
     * @DTA\Data(field="userStatus", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $user_status;
}
