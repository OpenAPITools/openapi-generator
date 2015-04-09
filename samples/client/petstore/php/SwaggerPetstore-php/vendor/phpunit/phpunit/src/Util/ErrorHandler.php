<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

// Workaround for http://bugs.php.net/bug.php?id=47987,
// see https://github.com/sebastianbergmann/phpunit/issues#issue/125 for details
require_once __DIR__ . '/../Framework/Error.php';
require_once __DIR__ . '/../Framework/Error/Notice.php';
require_once __DIR__ . '/../Framework/Error/Warning.php';
require_once __DIR__ . '/../Framework/Error/Deprecated.php';

/**
 * Error handler that converts PHP errors and warnings to exceptions.
 *
 * @package    PHPUnit
 * @subpackage Util
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.3.0
 */
class PHPUnit_Util_ErrorHandler
{
    protected static $errorStack = array();

    /**
     * Returns the error stack.
     *
     * @return array
     */
    public static function getErrorStack()
    {
        return self::$errorStack;
    }

    /**
     * @param  integer                 $errno
     * @param  string                  $errstr
     * @param  string                  $errfile
     * @param  integer                 $errline
     * @throws PHPUnit_Framework_Error
     */
    public static function handleError($errno, $errstr, $errfile, $errline)
    {
        if (!($errno & error_reporting())) {
            return false;
        }

        self::$errorStack[] = array($errno, $errstr, $errfile, $errline);

        $trace = debug_backtrace(false);
        array_shift($trace);

        foreach ($trace as $frame) {
            if ($frame['function'] == '__toString') {
                return false;
            }
        }

        if ($errno == E_NOTICE || $errno == E_USER_NOTICE || $errno == E_STRICT) {
            if (PHPUnit_Framework_Error_Notice::$enabled !== true) {
                return false;
            }

            $exception = 'PHPUnit_Framework_Error_Notice';
        } elseif ($errno == E_WARNING || $errno == E_USER_WARNING) {
            if (PHPUnit_Framework_Error_Warning::$enabled !== true) {
                return false;
            }

            $exception = 'PHPUnit_Framework_Error_Warning';
        } elseif ($errno == E_DEPRECATED || $errno == E_USER_DEPRECATED) {
            if (PHPUnit_Framework_Error_Deprecated::$enabled !== true) {
                return false;
            }

            $exception = 'PHPUnit_Framework_Error_Deprecated';
        } else {
            $exception = 'PHPUnit_Framework_Error';
        }

        throw new $exception($errstr, $errno, $errfile, $errline);
    }

    /**
     * Registers an error handler and returns a function that will restore
     * the previous handler when invoked
     * @param  integer   $severity PHP predefined error constant
     * @link   http://www.php.net/manual/en/errorfunc.constants.php
     * @throws Exception if event of specified severity is emitted
     */
    public static function handleErrorOnce($severity = E_WARNING)
    {
        $terminator = function () {
            static $expired = false;
            if (!$expired) {
                $expired = true;
                // cleans temporary error handler
                return restore_error_handler();
            }
        };

        set_error_handler(function ($errno, $errstr) use ($severity) {
            if ($errno === $severity) {
                return;
            }

            return false;
        });

        return $terminator;
    }
}
