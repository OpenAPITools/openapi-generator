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
 *
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Patrick Mueller <elias0@gmx.net>
 * @author     Frank Kleine <mikey@stubbles.net>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.0.0
 */
class Framework_MockObjectTest extends PHPUnit_Framework_TestCase
{
    public function testMockedMethodIsNeverCalled()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->never())
             ->method('doSomething');
    }

    public function testMockedMethodIsNeverCalledWithParameter()
    {
        $mock = $this->getMock('SomeClass');
        $mock->expects($this->never())
            ->method('doSomething')
            ->with('someArg');
    }

    public function testMockedMethodIsNotCalledWhenExpectsAnyWithParameter()
    {
        $mock = $this->getMock('SomeClass');
        $mock->expects($this->any())
             ->method('doSomethingElse')
             ->with('someArg');
    }

    public function testMockedMethodIsNotCalledWhenMethodSpecifiedDirectlyWithParameter()
    {
        $mock = $this->getMock('SomeClass');
        $mock->method('doSomethingElse')
            ->with('someArg');
    }

    public function testMockedMethodIsCalledAtLeastOnce()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->atLeastOnce())
             ->method('doSomething');

        $mock->doSomething();
    }

    public function testMockedMethodIsCalledAtLeastOnce2()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->atLeastOnce())
             ->method('doSomething');

        $mock->doSomething();
        $mock->doSomething();
    }

    public function testMockedMethodIsCalledAtLeastTwice()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->atLeast(2))
             ->method('doSomething');

        $mock->doSomething();
        $mock->doSomething();
    }

    public function testMockedMethodIsCalledAtLeastTwice2()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->atLeast(2))
             ->method('doSomething');

        $mock->doSomething();
        $mock->doSomething();
        $mock->doSomething();
    }

    public function testMockedMethodIsCalledAtMostTwice()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->atMost(2))
             ->method('doSomething');

        $mock->doSomething();
        $mock->doSomething();
    }

    public function testMockedMethodIsCalledAtMosttTwice2()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->atMost(2))
             ->method('doSomething');

        $mock->doSomething();
    }

    public function testMockedMethodIsCalledOnce()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->once())
             ->method('doSomething');

        $mock->doSomething();
    }

    public function testMockedMethodIsCalledOnceWithParameter()
    {
        $mock = $this->getMock('SomeClass');
        $mock->expects($this->once())
             ->method('doSomethingElse')
             ->with($this->equalTo('something'));

        $mock->doSomethingElse('something');
    }

    public function testMockedMethodIsCalledExactly()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->exactly(2))
             ->method('doSomething');

        $mock->doSomething();
        $mock->doSomething();
    }

    public function testStubbedException()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->will($this->throwException(new Exception));

        try {
            $mock->doSomething();
        } catch (Exception $e) {
            return;
        }

        $this->fail();
    }

    public function testStubbedWillThrowException()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->willThrowException(new Exception);

        try {
            $mock->doSomething();
        } catch (Exception $e) {
            return;
        }

        $this->fail();
    }

    public function testStubbedReturnValue()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->will($this->returnValue('something'));

        $this->assertEquals('something', $mock->doSomething());

        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->willReturn('something');

        $this->assertEquals('something', $mock->doSomething());
    }

    public function testStubbedReturnValueMap()
    {
        $map = array(
            array('a', 'b', 'c', 'd'),
            array('e', 'f', 'g', 'h')
        );

        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->will($this->returnValueMap($map));

        $this->assertEquals('d', $mock->doSomething('a', 'b', 'c'));
        $this->assertEquals('h', $mock->doSomething('e', 'f', 'g'));
        $this->assertEquals(NULL, $mock->doSomething('foo', 'bar'));

        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->willReturnMap($map);

        $this->assertEquals('d', $mock->doSomething('a', 'b', 'c'));
        $this->assertEquals('h', $mock->doSomething('e', 'f', 'g'));
        $this->assertEquals(NULL, $mock->doSomething('foo', 'bar'));
    }

    public function testStubbedReturnArgument()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->will($this->returnArgument(1));

        $this->assertEquals('b', $mock->doSomething('a', 'b'));

        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->willReturnArgument(1);

        $this->assertEquals('b', $mock->doSomething('a', 'b'));
    }

    public function testFunctionCallback()
    {
        $mock = $this->getMock('SomeClass', array('doSomething'), array(), '', FALSE);
        $mock->expects($this->once())
             ->method('doSomething')
             ->will($this->returnCallback('functionCallback'));

        $this->assertEquals('pass', $mock->doSomething('foo', 'bar'));

        $mock = $this->getMock('SomeClass', array('doSomething'), array(), '', FALSE);
        $mock->expects($this->once())
             ->method('doSomething')
             ->willReturnCallback('functionCallback');

        $this->assertEquals('pass', $mock->doSomething('foo', 'bar'));
    }

    public function testStubbedReturnSelf()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->will($this->returnSelf());

        $this->assertEquals($mock, $mock->doSomething());

        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->willReturnSelf();

        $this->assertEquals($mock, $mock->doSomething());
    }

    public function testStubbedReturnOnConsecutiveCalls()
    {
        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->will($this->onConsecutiveCalls('a', 'b', 'c'));

        $this->assertEquals('a', $mock->doSomething());
        $this->assertEquals('b', $mock->doSomething());
        $this->assertEquals('c', $mock->doSomething());

        $mock = $this->getMock('AnInterface');
        $mock->expects($this->any())
             ->method('doSomething')
             ->willReturnOnConsecutiveCalls('a', 'b', 'c');

        $this->assertEquals('a', $mock->doSomething());
        $this->assertEquals('b', $mock->doSomething());
        $this->assertEquals('c', $mock->doSomething());
    }

    public function testStaticMethodCallback()
    {
        $mock = $this->getMock('SomeClass', array('doSomething'), array(), '', FALSE);
        $mock->expects($this->once())
             ->method('doSomething')
             ->will($this->returnCallback(array('MethodCallback', 'staticCallback')));

        $this->assertEquals('pass', $mock->doSomething('foo', 'bar'));
    }

    public function testPublicMethodCallback()
    {
        $mock = $this->getMock('SomeClass', array('doSomething'), array(), '', FALSE);
        $mock->expects($this->once())
             ->method('doSomething')
             ->will($this->returnCallback(array(new MethodCallback, 'nonStaticCallback')));

        $this->assertEquals('pass', $mock->doSomething('foo', 'bar'));
    }

    public function testMockClassOnlyGeneratedOnce()
    {
        $mock1 = $this->getMock('AnInterface');
        $mock2 = $this->getMock('AnInterface');

        $this->assertEquals(get_class($mock1), get_class($mock2));
    }

    public function testMockClassDifferentForPartialMocks()
    {
        $mock1 = $this->getMock('PartialMockTestClass');
        $mock2 = $this->getMock('PartialMockTestClass', array('doSomething'));
        $mock3 = $this->getMock('PartialMockTestClass', array('doSomething'));
        $mock4 = $this->getMock('PartialMockTestClass', array('doAnotherThing'));
        $mock5 = $this->getMock('PartialMockTestClass', array('doAnotherThing'));

        $this->assertNotEquals(get_class($mock1), get_class($mock2));
        $this->assertNotEquals(get_class($mock1), get_class($mock3));
        $this->assertNotEquals(get_class($mock1), get_class($mock4));
        $this->assertNotEquals(get_class($mock1), get_class($mock5));
        $this->assertEquals(get_class($mock2), get_class($mock3));
        $this->assertNotEquals(get_class($mock2), get_class($mock4));
        $this->assertNotEquals(get_class($mock2), get_class($mock5));
        $this->assertEquals(get_class($mock4), get_class($mock5));
    }

    public function testMockClassStoreOverrulable()
    {
        $mock1 = $this->getMock('PartialMockTestClass');
        $mock2 = $this->getMock('PartialMockTestClass', array(), array(), 'MyMockClassNameForPartialMockTestClass1');
        $mock3 = $this->getMock('PartialMockTestClass');
        $mock4 = $this->getMock('PartialMockTestClass', array('doSomething'), array(), 'AnotherMockClassNameForPartialMockTestClass');
        $mock5 = $this->getMock('PartialMockTestClass', array(), array(), 'MyMockClassNameForPartialMockTestClass2');

        $this->assertNotEquals(get_class($mock1), get_class($mock2));
        $this->assertEquals(get_class($mock1), get_class($mock3));
        $this->assertNotEquals(get_class($mock1), get_class($mock4));
        $this->assertNotEquals(get_class($mock2), get_class($mock3));
        $this->assertNotEquals(get_class($mock2), get_class($mock4));
        $this->assertNotEquals(get_class($mock2), get_class($mock5));
        $this->assertNotEquals(get_class($mock3), get_class($mock4));
        $this->assertNotEquals(get_class($mock3), get_class($mock5));
        $this->assertNotEquals(get_class($mock4), get_class($mock5));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMock
     */
    public function testGetMockWithFixedClassNameCanProduceTheSameMockTwice()
    {
        $mock = $this->getMockBuilder('StdClass')->setMockClassName('FixedName')->getMock();
        $mock = $this->getMockBuilder('StdClass')->setMockClassName('FixedName')->getMock();
        $this->assertInstanceOf('StdClass', $mock);
    }

    public function testOriginalConstructorSettingConsidered()
    {
        $mock1 = $this->getMock('PartialMockTestClass');
        $mock2 = $this->getMock('PartialMockTestClass', array(), array(), '', FALSE);

        $this->assertTrue($mock1->constructorCalled);
        $this->assertFalse($mock2->constructorCalled);
    }

    public function testOriginalCloneSettingConsidered()
    {
        $mock1 = $this->getMock('PartialMockTestClass');
        $mock2 = $this->getMock('PartialMockTestClass', array(), array(), '', TRUE, FALSE);

        $this->assertNotEquals(get_class($mock1), get_class($mock2));
    }

    public function testGetMockForAbstractClass()
    {
        $mock = $this->getMock('AbstractMockTestClass');
        $mock->expects($this->never())
             ->method('doSomething');
    }

    public function traversableProvider()
    {
        return array(
          array('Traversable'),
          array('\Traversable'),
          array('TraversableMockTestInterface'),
          array(array('Traversable')),
          array(array('Iterator','Traversable')),
          array(array('\Iterator','\Traversable'))
        );
    }

    /**
     * @dataProvider traversableProvider
     */
    public function testGetMockForTraversable($type)
    {
        $mock = $this->getMock($type);
        $this->assertInstanceOf('Traversable', $mock);
    }

    public function testMultipleInterfacesCanBeMockedInSingleObject()
    {
        $mock = $this->getMock(array('AnInterface', 'AnotherInterface'));
        $this->assertInstanceOf('AnInterface', $mock);
        $this->assertInstanceOf('AnotherInterface', $mock);
    }

    /**
     * @requires PHP 5.4.0
     */
    public function testGetMockForTrait()
    {
        $mock = $this->getMockForTrait('AbstractTrait');
        $mock->expects($this->never())->method('doSomething');

        $parent = get_parent_class($mock);
        $traits = class_uses($parent, FALSE);

        $this->assertContains('AbstractTrait', $traits);
    }

    public function testClonedMockObjectShouldStillEqualTheOriginal()
    {
        $a = $this->getMock('stdClass');
        $b = clone $a;
        $this->assertEquals($a, $b);
    }

    public function testMockObjectsConstructedIndepentantlyShouldBeEqual()
    {
        $a = $this->getMock('stdClass');
        $b = $this->getMock('stdClass');
        $this->assertEquals($a, $b);
    }

    public function testMockObjectsConstructedIndepentantlyShouldNotBeTheSame()
    {
        $a = $this->getMock('stdClass');
        $b = $this->getMock('stdClass');
        $this->assertNotSame($a, $b);
    }

    public function testClonedMockObjectCanBeUsedInPlaceOfOriginalOne()
    {
        $x = $this->getMock('stdClass');
        $y = clone $x;

        $mock = $this->getMock('stdClass', array('foo'));
        $mock->expects($this->once())->method('foo')->with($this->equalTo($x));
        $mock->foo($y);
    }

    public function testClonedMockObjectIsNotIdenticalToOriginalOne()
    {
        $x = $this->getMock('stdClass');
        $y = clone $x;

        $mock = $this->getMock('stdClass', array('foo'));
        $mock->expects($this->once())->method('foo')->with($this->logicalNot($this->identicalTo($x)));
        $mock->foo($y);
    }

    public function testObjectMethodCallWithArgumentCloningEnabled()
    {
        $expectedObject = new StdClass;

        $mock = $this->getMockBuilder('SomeClass')
                     ->setMethods(array('doSomethingElse'))
                     ->enableArgumentCloning()
                     ->getMock();

        $actualArguments = array();

        $mock->expects($this->any())
        ->method('doSomethingElse')
        ->will($this->returnCallback(function () use (&$actualArguments) {
            $actualArguments = func_get_args();
        }));

        $mock->doSomethingElse($expectedObject);

        $this->assertEquals(1, count($actualArguments));
        $this->assertEquals($expectedObject, $actualArguments[0]);
        $this->assertNotSame($expectedObject, $actualArguments[0]);
    }

    public function testObjectMethodCallWithArgumentCloningDisabled()
    {
        $expectedObject = new StdClass;

        $mock = $this->getMockBuilder('SomeClass')
                     ->setMethods(array('doSomethingElse'))
                     ->disableArgumentCloning()
                     ->getMock();

        $actualArguments = array();

        $mock->expects($this->any())
        ->method('doSomethingElse')
        ->will($this->returnCallback(function () use (&$actualArguments) {
            $actualArguments = func_get_args();
        }));

        $mock->doSomethingElse($expectedObject);

        $this->assertEquals(1, count($actualArguments));
        $this->assertSame($expectedObject, $actualArguments[0]);
    }

    public function testArgumentCloningOptionGeneratesUniqueMock()
    {
        $mockWithCloning = $this->getMockBuilder('SomeClass')
                                ->setMethods(array('doSomethingElse'))
                                ->enableArgumentCloning()
                                ->getMock();

        $mockWithoutCloning = $this->getMockBuilder('SomeClass')
                                   ->setMethods(array('doSomethingElse'))
                                   ->disableArgumentCloning()
                                   ->getMock();

        $this->assertNotEquals($mockWithCloning, $mockWithoutCloning);
    }

    public function testVerificationOfMethodNameFailsWithoutParameters()
    {
        $mock = $this->getMock('SomeClass', array('right', 'wrong'), array(), '', TRUE, TRUE, TRUE);
        $mock->expects($this->once())
             ->method('right');

        $mock->wrong();
        try {
            $mock->__phpunit_verify();
            $this->fail('Expected exception');
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                "Expectation failed for method name is equal to <string:right> when invoked 1 time(s).\n"
                . "Method was expected to be called 1 times, actually called 0 times.\n",
                $e->getMessage()
            );
        }

        $this->resetMockObjects();
    }

    public function testVerificationOfMethodNameFailsWithParameters()
    {
        $mock = $this->getMock('SomeClass', array('right', 'wrong'), array(), '', TRUE, TRUE, TRUE);
        $mock->expects($this->once())
             ->method('right');

        $mock->wrong();
        try {
            $mock->__phpunit_verify();
            $this->fail('Expected exception');
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                "Expectation failed for method name is equal to <string:right> when invoked 1 time(s).\n"
                . "Method was expected to be called 1 times, actually called 0 times.\n",
                $e->getMessage()
            );
        }

        $this->resetMockObjects();
    }

    public function testVerificationOfMethodNameFailsWithWrongParameters()
    {
        $mock = $this->getMock('SomeClass', array('right', 'wrong'), array(), '', TRUE, TRUE, TRUE);
        $mock->expects($this->once())
             ->method('right')
             ->with(array('first', 'second'));

        try {
            $mock->right(array('second'));
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                "Expectation failed for method name is equal to <string:right> when invoked 1 time(s)\n"
                . "Parameter 0 for invocation SomeClass::right(Array (...)) does not match expected value.\n"
                . "Failed asserting that two arrays are equal.",
                $e->getMessage()
            );
        }

        try {
            $mock->__phpunit_verify();
            $this->fail('Expected exception');
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                "Expectation failed for method name is equal to <string:right> when invoked 1 time(s).\n"
                . "Parameter 0 for invocation SomeClass::right(Array (...)) does not match expected value.\n"
                . "Failed asserting that two arrays are equal.\n"
                . "--- Expected\n"
                . "+++ Actual\n"
                . "@@ @@\n"
                . " Array (\n"
                . "-    0 => 'first'\n"
                . "-    1 => 'second'\n"
                . "+    0 => 'second'\n"
                . " )\n",
                $e->getMessage()
            );
        }

        $this->resetMockObjects();
    }

    public function testVerificationOfNeverFailsWithEmptyParameters()
    {
        $mock = $this->getMock('SomeClass', array('right', 'wrong'), array(), '', TRUE, TRUE, TRUE);
        $mock->expects($this->never())
             ->method('right')
             ->with();

        try {
            $mock->right();
            $this->fail('Expected exception');
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                'SomeClass::right() was not expected to be called.',
                $e->getMessage()
            );
        }

        $this->resetMockObjects();
    }

    public function testVerificationOfNeverFailsWithAnyParameters()
    {
        $mock = $this->getMock('SomeClass', array('right', 'wrong'), array(), '', TRUE, TRUE, TRUE);
        $mock->expects($this->never())
             ->method('right')
             ->withAnyParameters();

        try {
            $mock->right();
            $this->fail('Expected exception');
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                'SomeClass::right() was not expected to be called.',
                $e->getMessage()
            );
        }

        $this->resetMockObjects();
    }

    /**
     * @ticket 199
     */
    public function testWithAnythingInsteadOfWithAnyParameters()
    {
        $mock = $this->getMock('SomeClass', array('right'), array(), '', TRUE, TRUE, TRUE);
        $mock->expects($this->once())
             ->method('right')
             ->with($this->anything());

        try {
            $mock->right();
            $this->fail('Expected exception');
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            $this->assertSame(
                "Expectation failed for method name is equal to <string:right> when invoked 1 time(s)\n" .
                "Parameter count for invocation SomeClass::right() is too low.\n" .
                "To allow 0 or more parameters with any value, omit ->with() or use ->withAnyParameters() instead.",
                $e->getMessage()
            );
        }

        $this->resetMockObjects();
    }

    /**
     * See https://github.com/sebastianbergmann/phpunit-mock-objects/issues/81
     */
    public function testMockArgumentsPassedByReference()
    {
        $foo = $this->getMockBuilder('MethodCallbackByReference')
                    ->setMethods(array('bar'))
                    ->disableOriginalConstructor()
                    ->disableArgumentCloning()
                    ->getMock();

        $foo->expects($this->any())
            ->method('bar')
            ->will($this->returnCallback(array($foo, 'callback')));

        $a = $b = $c = 0;

        $foo->bar($a, $b, $c);

        $this->assertEquals(1, $b);
    }

    /**
     * See https://github.com/sebastianbergmann/phpunit-mock-objects/issues/81
     */
    public function testMockArgumentsPassedByReference2()
    {
        $foo = $this->getMockBuilder('MethodCallbackByReference')
                    ->disableOriginalConstructor()
                    ->disableArgumentCloning()
                    ->getMock();

        $foo->expects($this->any())
            ->method('bar')
            ->will($this->returnCallback(
            function (&$a, &$b, $c) {
                $b = 1;
            }
            ));

        $a = $b = $c = 0;

        $foo->bar($a, $b, $c);

        $this->assertEquals(1, $b);
    }

    /**
     * https://github.com/sebastianbergmann/phpunit-mock-objects/issues/116
     */
    public function testMockArgumentsPassedByReference3()
    {
        $foo = $this->getMockBuilder('MethodCallbackByReference')
                    ->setMethods(array('bar'))
                    ->disableOriginalConstructor()
                    ->disableArgumentCloning()
                    ->getMock();

        $a = new stdClass();
        $b = $c = 0;

        $foo->expects($this->any())
            ->method('bar')
            ->with($a, $b, $c)
            ->will($this->returnCallback(array($foo, 'callback')));

        $foo->bar($a, $b, $c);
    }

    /**
     * https://github.com/sebastianbergmann/phpunit/issues/796
     */
    public function testMockArgumentsPassedByReference4()
    {
        $foo = $this->getMockBuilder('MethodCallbackByReference')
                    ->setMethods(array('bar'))
                    ->disableOriginalConstructor()
                    ->disableArgumentCloning()
                    ->getMock();

        $a = new stdClass();
        $b = $c = 0;

        $foo->expects($this->any())
            ->method('bar')
            ->with($this->isInstanceOf("stdClass"), $b, $c)
            ->will($this->returnCallback(array($foo, 'callback')));

        $foo->bar($a, $b, $c);
    }

    /**
     * @requires extension soap
     */
    public function testCreateMockFromWsdl()
    {
        $mock = $this->getMockFromWsdl(__DIR__ . '/_fixture/GoogleSearch.wsdl', 'WsdlMock');
        $this->assertStringStartsWith(
            'Mock_WsdlMock_',
            get_class($mock)
        );
    }

    /**
     * @requires extension soap
     */
    public function testCreateNamespacedMockFromWsdl()
    {
        $mock = $this->getMockFromWsdl(__DIR__ . '/_fixture/GoogleSearch.wsdl', 'My\\Space\\WsdlMock');
        $this->assertStringStartsWith(
            'Mock_WsdlMock_',
            get_class($mock)
        );
    }

    /**
     * @requires extension soap
     */
    public function testCreateTwoMocksOfOneWsdlFile()
    {
        $mock = $this->getMockFromWsdl(__DIR__ . '/_fixture/GoogleSearch.wsdl');
        $mock = $this->getMockFromWsdl(__DIR__ . '/_fixture/GoogleSearch.wsdl');
    }

    /**
     * @see    https://github.com/sebastianbergmann/phpunit-mock-objects/issues/156
     * @ticket 156
     */
    public function testInterfaceWithStaticMethodCanBeStubbed()
    {
        $this->assertInstanceOf(
            'InterfaceWithStaticMethod',
            $this->getMock('InterfaceWithStaticMethod')
        );
    }

    /**
     * @expectedException PHPUnit_Framework_MockObject_BadMethodCallException
     */
    public function testInvokingStubbedStaticMethodRaisesException()
    {
        $mock = $this->getMock('ClassWithStaticMethod');
        $mock->staticMethod();
    }

    /**
     * @see    https://github.com/sebastianbergmann/phpunit-mock-objects/issues/171
     * @ticket 171
     */
    public function testStubForClassThatImplementsSerializableCanBeCreatedWithoutInvokingTheConstructor()
    {
        $this->assertInstanceOf(
            'ClassThatImplementsSerializable',
            $this->getMockBuilder('ClassThatImplementsSerializable')
                 ->disableOriginalConstructor()
                 ->getMock()
        );
    }

    private function resetMockObjects()
    {
        $refl = new ReflectionObject($this);
        $refl = $refl->getParentClass();
        $prop = $refl->getProperty('mockObjects');
        $prop->setAccessible(true);
        $prop->setValue($this, array());
    }
}
