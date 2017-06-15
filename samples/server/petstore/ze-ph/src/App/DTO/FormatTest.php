<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class FormatTest 
{
    /**
     * @DTA\Data(field="integer", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @DTA\Validator(name="GreaterThan", options={"min":10, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":100, "inclusive":true})
     * @var int
     */
    public $integer;
    /**
     * @DTA\Data(field="int32", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @DTA\Validator(name="GreaterThan", options={"min":20, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":200, "inclusive":true})
     * @var int
     */
    public $int32;
    /**
     * @DTA\Data(field="int64", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"int"})
     * @var int
     */
    public $int64;
    /**
     * @DTA\Data(field="number")
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @DTA\Validator(name="GreaterThan", options={"min":32.1, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":543.2, "inclusive":true})
     * @var float
     */
    public $number;
    /**
     * @DTA\Data(field="float", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @DTA\Validator(name="GreaterThan", options={"min":54.3, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":987.6, "inclusive":true})
     * @var float
     */
    public $float;
    /**
     * @DTA\Data(field="double", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"float"})
     * @DTA\Validator(name="GreaterThan", options={"min":67.8, "inclusive":true})
     * @DTA\Validator(name="LessThan", options={"max":123.4, "inclusive":true})
     * @var double
     */
    public $double;
    /**
     * @DTA\Data(field="string", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="Regex", options={"pattern":"/[a-z]/i"})
     * @var string
     */
    public $string;
    /**
     * @DTA\Data(field="byte")
     * @var string
     */
    public $byte;
    /**
     * @DTA\Data(field="binary", nullable=true)
     * @var string
     */
    public $binary;
    /**
     * @DTA\Data(field="date")
     * @DTA\Strategy(name="Date")
     * @DTA\Validator(name="Date")
     * @var \DateTime
     */
    public $date;
    /**
     * @DTA\Data(field="dateTime", nullable=true)
     * @DTA\Strategy(name="DateTime")
     * @DTA\Validator(name="Date", options={"format": \DateTime::RFC3339})
     * @var \DateTime
     */
    public $date_time;
    /**
     * @DTA\Data(field="uuid", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $uuid;
    /**
     * @DTA\Data(field="password")
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="StringLength", options={"min":10, "max":64})
     * @var string
     */
    public $password;
}

