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
 * Builder interface for matcher of method names.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Interface available since Release 1.0.0
 */
interface PHPUnit_Framework_MockObject_Builder_MethodNameMatch extends PHPUnit_Framework_MockObject_Builder_ParametersMatch
{
    /**
     * Adds a new method name match and returns the parameter match object for
     * further matching possibilities.
     *
     * @param  PHPUnit_Framework_Constraint                         $name
     *                                                                    Constraint for matching method, if a string is passed it will use
     *                                                                    the PHPUnit_Framework_Constraint_IsEqual.
     * @return PHPUnit_Framework_MockObject_Builder_ParametersMatch
     */
    public function method($name);
}
