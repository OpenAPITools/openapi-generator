<?php
/*
 * This file is part of the PHPUnit_MockObject package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Records invocations and provides convenience methods for checking them later
 * on.
 * This abstract class can be implemented by matchers which needs to check the
 * number of times an invocation has occured.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 * @abstract
 */
abstract class PHPUnit_Framework_MockObject_Matcher_InvokedRecorder implements PHPUnit_Framework_MockObject_Matcher_Invocation
{
    /**
     * @var PHPUnit_Framework_MockObject_Invocation[]
     */
    protected $invocations = array();

    /**
     * @return integer
     */
    public function getInvocationCount()
    {
        return count($this->invocations);
    }

    /**
     * @return PHPUnit_Framework_MockObject_Invocation[]
     */
    public function getInvocations()
    {
        return $this->invocations;
    }

    /**
     * @return boolean
     */
    public function hasBeenInvoked()
    {
        return count($this->invocations) > 0;
    }

    /**
     * @param PHPUnit_Framework_MockObject_Invocation $invocation
     */
    public function invoked(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        $this->invocations[] = $invocation;
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     * @return boolean
     */
    public function matches(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        return TRUE;
    }
}
