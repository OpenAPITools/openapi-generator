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
 * Builder interface for parameter matchers.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Interface available since Release 1.0.0
 */
interface PHPUnit_Framework_MockObject_Builder_ParametersMatch extends PHPUnit_Framework_MockObject_Builder_Match
{
    /**
     * Sets the parameters to match for, each parameter to this funtion will
     * be part of match. To perform specific matches or constraints create a
     * new PHPUnit_Framework_Constraint and use it for the parameter.
     * If the parameter value is not a constraint it will use the
     * PHPUnit_Framework_Constraint_IsEqual for the value.
     *
     * Some examples:
     * <code>
     * // match first parameter with value 2
     * $b->with(2);
     * // match first parameter with value 'smock' and second identical to 42
     * $b->with('smock', new PHPUnit_Framework_Constraint_IsEqual(42));
     * </code>
     *
     * @return PHPUnit_Framework_MockObject_Builder_ParametersMatch
     */
    public function with();

    /**
     * Sets a matcher which allows any kind of parameters.
     *
     * Some examples:
     * <code>
     * // match any number of parameters
     * $b->withAnyParamers();
     * </code>
     *
     * @return PHPUnit_Framework_MockObject_Matcher_AnyParameters
     */
    public function withAnyParameters();
}
