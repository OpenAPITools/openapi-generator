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
 * @package    PHPUnit_MockObject
 * @author     Giorgio Sironi <piccoloprincipeazzurro@gmail.com>
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      File available since Release 1.0.0
 */
class Framework_MockBuilderTest extends PHPUnit_Framework_TestCase
{
    public function testMockBuilderRequiresClassName()
    {
        $spec = $this->getMockBuilder('Mockable');
        $mock = $spec->getMock();
        $this->assertTrue($mock instanceof Mockable);
    }

    public function testByDefaultMocksAllMethods()
    {
        $spec = $this->getMockBuilder('Mockable');
        $mock = $spec->getMock();
        $this->assertNull($mock->mockableMethod());
        $this->assertNull($mock->anotherMockableMethod());
    }

    public function testMethodsToMockCanBeSpecified()
    {
        $spec = $this->getMockBuilder('Mockable');
        $spec->setMethods(array('mockableMethod'));
        $mock = $spec->getMock();
        $this->assertNull($mock->mockableMethod());
        $this->assertTrue($mock->anotherMockableMethod());
    }

    public function testByDefaultDoesNotPassArgumentsToTheConstructor()
    {
        $spec = $this->getMockBuilder('Mockable');
        $mock = $spec->getMock();
        $this->assertEquals(array(NULL, NULL), $mock->constructorArgs);
    }

    public function testMockClassNameCanBeSpecified()
    {
        $spec = $this->getMockBuilder('Mockable');
        $spec->setMockClassName('ACustomClassName');
        $mock = $spec->getMock();
        $this->assertTrue($mock instanceof ACustomClassName);
    }

    public function testConstructorArgumentsCanBeSpecified()
    {
        $spec = $this->getMockBuilder('Mockable');
        $spec->setConstructorArgs($expected = array(23, 42));
        $mock = $spec->getMock();
        $this->assertEquals($expected, $mock->constructorArgs);
    }

    public function testOriginalConstructorCanBeDisabled()
    {
        $spec = $this->getMockBuilder('Mockable');
        $spec->disableOriginalConstructor();
        $mock = $spec->getMock();
        $this->assertNull($mock->constructorArgs);
    }

    public function testByDefaultOriginalCloneIsPreserved()
    {
        $spec = $this->getMockBuilder('Mockable');
        $mock = $spec->getMock();
        $cloned = clone $mock;
        $this->assertTrue($cloned->cloned);
    }

    public function testOriginalCloneCanBeDisabled()
    {
        $spec = $this->getMockBuilder('Mockable');
        $spec->disableOriginalClone();
        $mock = $spec->getMock();
        $mock->cloned = FALSE;
        $cloned = clone $mock;
        $this->assertFalse($cloned->cloned);
    }

    public function testCallingAutoloadCanBeDisabled()
    {
        // it is not clear to me how to test this nor the difference
        // between calling it or not
        $this->markTestIncomplete();
    }

    public function testProvidesAFluentInterface()
    {
        $spec = $this->getMockBuilder('Mockable')
                     ->setMethods(array('mockableMethod'))
                     ->setConstructorArgs(array())
                     ->setMockClassName('DummyClassName')
                     ->disableOriginalConstructor()
                     ->disableOriginalClone()
                     ->disableAutoload();
        $this->assertTrue($spec instanceof PHPUnit_Framework_MockObject_MockBuilder);
    }
}
