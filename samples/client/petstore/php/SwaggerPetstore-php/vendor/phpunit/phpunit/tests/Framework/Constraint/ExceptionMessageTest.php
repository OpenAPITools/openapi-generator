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
 *
 *
 * @package    PHPUnit
 * @author     Márcio Almada <marcio3w@gmail.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.0.20
 * @covers     PHPUnit_Framework_Constraint_ExceptionMessage
 */
class ExceptionMessageTest extends PHPUnit_Framework_TestCase
{

    /**
     * @expectedException \Exception
     * @expectedExceptionMessage A literal exception message
     */
    public function testLiteralMessage()
    {
        throw new Exception("A literal exception message");
    }

    /**
     * @expectedException \Exception
     * @expectedExceptionMessage A partial
     */
    public function testPatialMessageBegin()
    {
        throw new Exception("A partial exception message");
    }

    /**
     * @expectedException \Exception
     * @expectedExceptionMessage partial exception
     */
    public function testPatialMessageMiddle()
    {
        throw new Exception("A partial exception message");
    }

    /**
     * @expectedException \Exception
     * @expectedExceptionMessage exception message
     */
    public function testPatialMessageEnd()
    {
        throw new Exception("A partial exception message");
    }
}
