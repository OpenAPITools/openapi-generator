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
 * Interface for classes which matches an invocation based on its
 * method name, argument, order or call count.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Interface available since Release 1.0.0
 */
interface PHPUnit_Framework_MockObject_Matcher_Invocation extends PHPUnit_Framework_SelfDescribing, PHPUnit_Framework_MockObject_Verifiable
{
    /**
     * Registers the invocation $invocation in the object as being invoked.
     * This will only occur after matches() returns true which means the
     * current invocation is the correct one.
     *
     * The matcher can store information from the invocation which can later
     * be checked in verify(), or it can check the values directly and throw
     * and exception if an expectation is not met.
     *
     * If the matcher is a stub it will also have a return value.
     *
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     *                                                             Object containing information on a mocked or stubbed method which
     *                                                             was invoked.
     * @return mixed
     */
    public function invoked(PHPUnit_Framework_MockObject_Invocation $invocation);

    /**
     * Checks if the invocation $invocation matches the current rules. If it does
     * the matcher will get the invoked() method called which should check if an
     * expectation is met.
     *
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     *                                                             Object containing information on a mocked or stubbed method which
     *                                                             was invoked.
     * @return bool
     */
    public function matches(PHPUnit_Framework_MockObject_Invocation $invocation);
}
