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
 * A skipped test case
 *
 * @package    PHPUnit
 * @subpackage Framework
 * @author     Davey Shafik <me@daveyshafik.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.3.0
 */
class PHPUnit_Framework_SkippedTestCase extends PHPUnit_Framework_TestCase
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
     * @var boolean
     */
    protected $useOutputBuffering = false;

    /**
     * @param string $message
     */
    public function __construct($className, $methodName, $message = '')
    {
        $this->message = $message;
        parent::__construct($className . '::' . $methodName);
    }

    /**
     * @throws PHPUnit_Framework_Exception
     */
    protected function runTest()
    {
        $this->markTestSkipped($this->message);
    }

    /**
     * @return string
     */
    public function getMessage()
    {
        return $this->message;
    }

    /**
     * Returns a string representation of the test case.
     *
     * @return string
     */
    public function toString()
    {
        return $this->getName();
    }
}
