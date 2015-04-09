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
 * Creates a synthetic failed assertion.
 *
 * @package    PHPUnit
 * @subpackage Framework
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.5.0
 */
class PHPUnit_Framework_SyntheticError extends PHPUnit_Framework_AssertionFailedError
{
    /**
     * The synthetic file.
     *
     * @var string
     */
    protected $syntheticFile = '';

    /**
     * The synthetic line number.
     *
     * @var integer
     */
    protected $syntheticLine = 0;

    /**
     * The synthetic trace.
     *
     * @var array
     */
    protected $syntheticTrace = array();

    /**
     * Constructor.
     *
     * @param string  $message
     * @param integer $code
     * @param string  $file
     * @param integer $line
     * @param array   $trace
     */
    public function __construct($message, $code, $file, $line, $trace)
    {
        parent::__construct($message, $code);

        $this->syntheticFile  = $file;
        $this->syntheticLine  = $line;
        $this->syntheticTrace = $trace;
    }

    /**
     * @return string
     */
    public function getSyntheticFile()
    {
        return $this->syntheticFile;
    }

    /**
     * @return integer
     */
    public function getSyntheticLine()
    {
        return $this->syntheticLine;
    }

    /**
     * @return array
     */
    public function getSyntheticTrace()
    {
        return $this->syntheticTrace;
    }
}
