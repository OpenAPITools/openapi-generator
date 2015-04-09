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
 * Builder interface for stubs which are actions replacing an invocation.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Interface available since Release 1.0.0
 */
interface PHPUnit_Framework_MockObject_Builder_Stub extends PHPUnit_Framework_MockObject_Builder_Identity
{
    /**
     * Stubs the matching method with the stub object $stub. Any invocations of
     * the matched method will now be handled by the stub instead.
     *
     * @param  PHPUnit_Framework_MockObject_Stub             $stub The stub object.
     * @return PHPUnit_Framework_MockObject_Builder_Identity
     */
    public function will(PHPUnit_Framework_MockObject_Stub $stub);
}
