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
 * Stubs a method by returning the current object.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Kris Wallsmith <kris.wallsmith@gmail.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.1.0
 */
class PHPUnit_Framework_MockObject_Stub_ReturnSelf implements PHPUnit_Framework_MockObject_Stub
{
    public function invoke(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        if (!$invocation instanceof PHPUnit_Framework_MockObject_Invocation_Object) {
            throw new PHPUnit_Framework_Exception(
                'The current object can only be returned when mocking an ' .
                'object, not a static class.'
            );
        }

        return $invocation->object;
    }

    public function toString()
    {
        return 'return the current object';
    }
}
