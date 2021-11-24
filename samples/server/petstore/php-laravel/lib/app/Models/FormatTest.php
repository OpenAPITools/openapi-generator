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
    private $integer;

    /** @var int $int32 */
    private $int32;

    /** @var int $int64 */
    private $int64;

    /** @var float $number */
    private $number;

    /** @var float $float */
    private $float;

    /** @var double $double */
    private $double;

    /** @var float $decimal */
    private $decimal;

    /** @var string $string */
    private $string;

    /** @var string $byte */
    private $byte;

    /** @var \SplFileObject $binary */
    private $binary;

    /** @var \DateTime $date */
    private $date;

    /** @var \DateTime $dateTime */
    private $dateTime;

    /** @var string $uuid */
    private $uuid;

    /** @var string $password */
    private $password;

    /** @var string $patternWithDigits A string that is a 10 digit number. Can have leading zeros.*/
    private $patternWithDigits;

    /** @var string $patternWithDigitsAndDelimiter A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.*/
    private $patternWithDigitsAndDelimiter;

}
