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
 * Invocation matcher which allows any parameters to a method.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Matcher_AnyParameters extends PHPUnit_Framework_MockObject_Matcher_StatelessInvocation
{
    /**
     * @return string
     */
    public function toString()
    {
        return 'with any parameters';
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
