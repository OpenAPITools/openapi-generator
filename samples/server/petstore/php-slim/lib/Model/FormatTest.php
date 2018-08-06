<?php
/**
 * FormatTest
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * FormatTest
 */
class FormatTest
{

    /** @var float $number */
    private $number;

    /** @var string $byte */
    private $byte;

    /** @var \DateTime $date */
    private $date;

    /** @var string $password */
    private $password;

    /** @var int $integer (optional) */
    private $integer;

    /** @var int $int32 (optional) */
    private $int32;

    /** @var int $int64 (optional) */
    private $int64;

    /** @var float $float (optional) */
    private $float;

    /** @var double $double (optional) */
    private $double;

    /** @var string $string (optional) */
    private $string;

    /** @var \SplFileObject $binary (optional) */
    private $binary;

    /** @var \DateTime $dateTime (optional) */
    private $dateTime;

    /** @var string $uuid (optional) */
    private $uuid;

    /**
     * FormatTest constructor
     *
     * @param float $number
     * @param string $byte
     * @param \DateTime $date
     * @param string $password
     * @param int|null $integer (optional)
     * @param int|null $int32 (optional)
     * @param int|null $int64 (optional)
     * @param float|null $float (optional)
     * @param double|null $double (optional)
     * @param string|null $string (optional)
     * @param \SplFileObject|null $binary (optional)
     * @param \DateTime|null $dateTime (optional)
     * @param string|null $uuid (optional)
     */
    public function __construct(
        $number,
        $byte,
        $date,
        $password,
        $integer = null,
        $int32 = null,
        $int64 = null,
        $float = null,
        $double = null,
        $string = null,
        $binary = null,
        $dateTime = null,
        $uuid = null,
    ) {
        $this->integer = $integer;
        $this->int32 = $int32;
        $this->int64 = $int64;
        $this->number = $number;
        $this->float = $float;
        $this->double = $double;
        $this->string = $string;
        $this->byte = $byte;
        $this->binary = $binary;
        $this->date = $date;
        $this->dateTime = $dateTime;
        $this->uuid = $uuid;
        $this->password = $password;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $formatTest = FormatTest::createFromObject([ 'integer' => 'foobar' ]);
     *
     * @return FormatTest
     */
    public static function createFromObject(array $data = null)
    {
        if ($data['number'] === null) {
            throw new InvalidArgumentException("'number' can't be null");
        }
        $number = $data['number'];
        if ($data['byte'] === null) {
            throw new InvalidArgumentException("'byte' can't be null");
        }
        $byte = $data['byte'];
        if ($data['date'] === null) {
            throw new InvalidArgumentException("'date' can't be null");
        }
        $date = $data['date'];
        if ($data['password'] === null) {
            throw new InvalidArgumentException("'password' can't be null");
        }
        $password = $data['password'];
        $integer = (isset($data['integer'])) ? $data['integer'] : null;
        $int32 = (isset($data['int32'])) ? $data['int32'] : null;
        $int64 = (isset($data['int64'])) ? $data['int64'] : null;
        $float = (isset($data['float'])) ? $data['float'] : null;
        $double = (isset($data['double'])) ? $data['double'] : null;
        $string = (isset($data['string'])) ? $data['string'] : null;
        $binary = (isset($data['binary'])) ? $data['binary'] : null;
        $dateTime = (isset($data['dateTime'])) ? $data['dateTime'] : null;
        $uuid = (isset($data['uuid'])) ? $data['uuid'] : null;
        return new FormatTest(
            $number,
            $byte,
            $date,
            $password,
            $integer,
            $int32,
            $int64,
            $float,
            $double,
            $string,
            $binary,
            $dateTime,
            $uuid,
        );
    }
}
