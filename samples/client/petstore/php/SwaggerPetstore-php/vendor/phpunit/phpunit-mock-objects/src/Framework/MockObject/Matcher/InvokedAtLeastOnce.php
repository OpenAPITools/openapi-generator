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
 * Invocation matcher which checks if a method has been invoked at least one
 * time.
 *
 * If the number of invocations is 0 it will throw an exception in verify.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Matcher_InvokedAtLeastOnce extends PHPUnit_Framework_MockObject_Matcher_InvokedRecorder
{
    /**
     * @return string
     */
    public function toString()
    {
        return 'invoked at least once';
    }

    /**
     * Verifies that the current expectation is valid. If everything is OK the
     * code should just return, if not it must throw an exception.
     *
     * @throws PHPUnit_Framework_ExpectationFailedException
     */
    public function verify()
    {
        $count = $this->getInvocationCount();

        if ($count < 1) {
            throw new PHPUnit_Framework_ExpectationFailedException(
              'Expected invocation at least once but it never occured.'
            );
        }
    }
}
