<?php
/**
 * FormatTest
 */
namespace app\Models;

/**
 * FormatTest
 */
class FormatTest {

    /** @var int $integer */
    public $integer = 0;

    /** @var int $int32 */
    public $int32 = 0;

    /** @var int $int64 */
    public $int64 = 0;

    /** @var float $number */
    public $number = 0;

    /** @var float $float */
    public $float = 0;

    /** @var float $double */
    public $double = 0;

    /** @var float $decimal */
    public $decimal = "";

    /** @var string $string */
    public $string = "";

    /** @var string $byte */
    public $byte = "";

    /** @var \SplFileObject $binary */
    public $binary;

    /** @var \DateTime $date */
    public $date;

    /** @var \DateTime $dateTime */
    public $dateTime;

    /** @var string $uuid */
    public $uuid = "";

    /** @var string $password */
    public $password = "";

    /** @var string $patternWithDigits A string that is a 10 digit number. Can have leading zeros.*/
    public $patternWithDigits = "";

    /** @var string $patternWithDigitsAndDelimiter A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.*/
    public $patternWithDigitsAndDelimiter = "";

}
