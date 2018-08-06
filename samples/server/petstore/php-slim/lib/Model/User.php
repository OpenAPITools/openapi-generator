<?php
/**
 * User
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * User
 */
class User
{

    /** @var int $id (optional) */
    private $id;

    /** @var string $username (optional) */
    private $username;

    /** @var string $firstName (optional) */
    private $firstName;

    /** @var string $lastName (optional) */
    private $lastName;

    /** @var string $email (optional) */
    private $email;

    /** @var string $password (optional) */
    private $password;

    /** @var string $phone (optional) */
    private $phone;

    /** @var int $userStatus (optional) User Status */
    private $userStatus;

    /**
     * User constructor
     *
     * @param int|null $id (optional)
     * @param string|null $username (optional)
     * @param string|null $firstName (optional)
     * @param string|null $lastName (optional)
     * @param string|null $email (optional)
     * @param string|null $password (optional)
     * @param string|null $phone (optional)
     * @param int|null $userStatus (optional) User Status
     */
    public function __construct(
        $id = null,
        $username = null,
        $firstName = null,
        $lastName = null,
        $email = null,
        $password = null,
        $phone = null,
        $userStatus = null
    ) {
        $this->id = $id;
        $this->username = $username;
        $this->firstName = $firstName;
        $this->lastName = $lastName;
        $this->email = $email;
        $this->password = $password;
        $this->phone = $phone;
        $this->userStatus = $userStatus;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $user = User::createFromObject([ 'id' => 'foobar' ]);
     *
     * @return User
     */
    public static function createFromObject(array $data = null)
    {
        $id = (isset($data['id'])) ? $data['id'] : null;
        $username = (isset($data['username'])) ? $data['username'] : null;
        $firstName = (isset($data['firstName'])) ? $data['firstName'] : null;
        $lastName = (isset($data['lastName'])) ? $data['lastName'] : null;
        $email = (isset($data['email'])) ? $data['email'] : null;
        $password = (isset($data['password'])) ? $data['password'] : null;
        $phone = (isset($data['phone'])) ? $data['phone'] : null;
        $userStatus = (isset($data['userStatus'])) ? $data['userStatus'] : null;
        return new User(
            $id,
            $username,
            $firstName,
            $lastName,
            $email,
            $password,
            $phone,
            $userStatus
        );
    }
}
