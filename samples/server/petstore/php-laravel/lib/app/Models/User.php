<?php
/**
 * User
 */
namespace app\Models;

/**
 * User
 */
class User {

    /** @var int $id */
    public $id = 0;

    /** @var string $username */
    public $username = "";

    /** @var string $firstName */
    public $firstName = "";

    /** @var string $lastName */
    public $lastName = "";

    /** @var string $email */
    public $email = "";

    /** @var string $password */
    public $password = "";

    /** @var string $phone */
    public $phone = "";

    /** @var int $userStatus User Status*/
    public $userStatus = 0;

}
