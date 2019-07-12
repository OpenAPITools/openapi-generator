<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class InlineObject3
{
    /**
     * None
     * @DTA\Data(field="integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @DTA\Validator(name="GreaterThan", options={"min":10, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":100, "inclusive":true})
     * @var int
     */
    public $integer;
    /**
     * None
     * @DTA\Data(field="int32", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @DTA\Validator(name="GreaterThan", options={"min":20, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":200, "inclusive":true})
     * @var int
     */
    public $int32;
    /**
     * None
     * @DTA\Data(field="int64", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $int64;
    /**
     * None
     * @DTA\Data(field="number")
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @DTA\Validator(name="GreaterThan", options={"min":32.1, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":543.2, "inclusive":true})
     * @var float
     */
    public $number;
    /**
     * None
     * @DTA\Data(field="float", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @DTA\Validator(name="LessThan", options={"max":987.6, "inclusive":true})
     * @var float
     */
    public $float;
    /**
     * None
     * @DTA\Data(field="double")
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @DTA\Validator(name="GreaterThan", options={"min":67.8, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":123.4, "inclusive":true})
     * @var float
     */
    public $double;
    /**
     * None
     * @DTA\Data(field="string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="Regex", options={"pattern":"/[a-z]/i"})
     * @var string
     */
    public $string;
    /**
     * None
     * @DTA\Data(field="pattern_without_delimiter")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="Regex", options={"pattern":"/^[A-Z].*/"})
     * @var string
     */
    public $pattern_without_delimiter;
    /**
     * None
     * @DTA\Data(field="byte")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $byte;
    /**
     * None
     * @DTA\Data(field="binary", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\SplFileObject::class})
     * @DTA\Validator(name="Dictionary", options={"type":\SplFileObject::class})
     * @var \SplFileObject
     */
    public $binary;
    /**
     * None
     * @DTA\Data(field="date", nullable=true)
     * @DTA\Strategy(name="Date")
     * @DTA\Validator(name="Date")
     * @var \DateTime
     */
    public $date;
    /**
     * None
     * @DTA\Data(field="dateTime", nullable=true)
     * @DTA\Strategy(name="DateTime")
     * @DTA\Validator(name="Date", options={"format": \DateTime::RFC3339})
     * @var \DateTime
     */
    public $date_time;
    /**
     * None
     * @DTA\Data(field="password", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="StringLength", options={"min":10, "max":64})
     * @var string
     */
    public $password;
    /**
     * None
     * @DTA\Data(field="callback", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $callback;
}
