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
 * Wrapper for PHP errors.
 *
 * @package    PHPUnit
 * @subpackage Framework
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.2.0
 */
class PHPUnit_Framework_Error extends PHPUnit_Framework_Exception
{
    /**
     * Constructor.
     *
     * @param string    $message
     * @param integer   $code
     * @param string    $file
     * @param integer   $line
     * @param Exception $previous
     */
    public function __construct($message, $code, $file, $line, Exception $previous = null)
    {
        parent::__construct($message, $code, $previous);

        $this->file  = $file;
        $this->line  = $line;
    }
}
