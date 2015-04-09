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
 * Represents a non-static invocation.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Invocation_Object extends PHPUnit_Framework_MockObject_Invocation_Static
{
    /**
     * @var object
     */
    public $object;

    /**
     * @param string $className
     * @param string $methodname
     * @param array  $parameters
     * @param object $object
     * @param object $cloneObjects
     */
    public function __construct($className, $methodName, array $parameters, $object, $cloneObjects = FALSE)
    {
        parent::__construct($className, $methodName, $parameters, $cloneObjects);
        $this->object = $object;
    }
}
