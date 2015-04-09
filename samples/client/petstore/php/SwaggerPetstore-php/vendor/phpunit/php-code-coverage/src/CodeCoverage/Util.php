<?php
/*
 * This file is part of the PHP_CodeCoverage package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Utility methods.
 *
 * @category   PHP
 * @package    CodeCoverage
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 1.0.0
 */
class PHP_CodeCoverage_Util
{
    /**
     * @param  float $a
     * @param  float $b
     * @return float ($a / $b) * 100
     */
    public static function percent($a, $b, $asString = false, $fixedWidth = false)
    {
        if ($asString && $b == 0) {
            return '';
        }

        if ($b > 0) {
            $percent = ($a / $b) * 100;
        } else {
            $percent = 100;
        }

        if ($asString) {
            if ($fixedWidth) {
                return sprintf('%6.2F%%', $percent);
            }

            return sprintf('%01.2F%%', $percent);
        } else {
            return $percent;
        }
    }
}
