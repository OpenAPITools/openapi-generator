<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * String helpers.
 *
 * @package    PHPUnit
 * @subpackage Util
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.6.0
 */
class PHPUnit_Util_String
{
    /**
     * Converts a string to UTF-8 encoding.
     *
     * @param  string $string
     * @return string
     */
    public static function convertToUtf8($string)
    {
        if (!self::isUtf8($string)) {
            if (function_exists('mb_convert_encoding')) {
                $string = mb_convert_encoding($string, 'UTF-8');
            } else {
                $string = utf8_encode($string);
            }
        }

        return $string;
    }

    /**
     * Checks a string for UTF-8 encoding.
     *
     * @param  string  $string
     * @return boolean
     */
    protected static function isUtf8($string)
    {
        $length = strlen($string);

        for ($i = 0; $i < $length; $i++) {
            if (ord($string[$i]) < 0x80) {
                $n = 0;
            } elseif ((ord($string[$i]) & 0xE0) == 0xC0) {
                $n = 1;
            } elseif ((ord($string[$i]) & 0xF0) == 0xE0) {
                $n = 2;
            } elseif ((ord($string[$i]) & 0xF0) == 0xF0) {
                $n = 3;
            } else {
                return false;
            }

            for ($j = 0; $j < $n; $j++) {
                if ((++$i == $length) || ((ord($string[$i]) & 0xC0) != 0x80)) {
                    return false;
                }
            }
        }

        return true;
    }
}
