<?php
declare(strict_types=1);

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
     * @var float
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
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $byte;
    /**
     * @DTA\Data(field="binary", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\SplFileObject::class})
     * @DTA\Validator(name="Dictionary", options={"type":\SplFileObject::class})
     * @var \SplFileObject
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
    /**
     * A string that is a 10 digit number. Can have leading zeros.
     * @DTA\Data(field="pattern_with_digits", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="Regex", options={"pattern":"/^\\d{10}$/"})
     * @var string
     */
    public $pattern_with_digits;
    /**
     * A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.
     * @DTA\Data(field="pattern_with_digits_and_delimiter", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @DTA\Validator(name="Regex", options={"pattern":"/^image_\\d{1,3}$/i"})
     * @var string
     */
    public $pattern_with_digits_and_delimiter;
}
