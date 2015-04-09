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
 * A warning.
 *
 * @package    PHPUnit
 * @subpackage Framework
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.0.0
 */
class PHPUnit_Framework_Warning extends PHPUnit_Framework_TestCase
{
    /**
     * @var string
     */
    protected $message = '';

    /**
     * @var boolean
     */
    protected $backupGlobals = false;

    /**
     * @var boolean
     */
    protected $backupStaticAttributes = false;

    /**
     * @var boolean
     */
    protected $runTestInSeparateProcess = false;

    /**
     * @var boolean
     */
    protected $useErrorHandler = false;

    /**
     * @param string $message
     */
    public function __construct($message = '')
    {
        $this->message = $message;
        parent::__construct('Warning');
    }

    /**
     * @throws PHPUnit_Framework_Exception
     */
    protected function runTest()
    {
        $this->fail($this->message);
    }

    /**
     * @return string
     * @since  Method available since Release 3.0.0
     */
    public function getMessage()
    {
        return $this->message;
    }

    /**
     * Returns a string representation of the test case.
     *
     * @return string
     * @since  Method available since Release 3.4.0
     */
    public function toString()
    {
        return 'Warning';
    }
}
