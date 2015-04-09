<?php
class Framework_MockObject_GeneratorTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var PHPUnit_Framework_MockObject_Generator
     */
    protected $generator;

    protected function setUp()
    {
        $this->generator = new PHPUnit_Framework_MockObject_Generator;
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMock
     * @expectedException PHPUnit_Framework_Exception
     */
    public function testGetMockFailsWhenInvalidFunctionNameIsPassedInAsAFunctionToMock()
    {
        $this->generator->getMock('StdClass', array(0));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMock
     */
    public function testGetMockCanCreateNonExistingFunctions()
    {
        $mock = $this->generator->getMock('StdClass', array('testFunction'));
        $this->assertTrue(method_exists($mock, 'testFunction'));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMock
     * @expectedException PHPUnit_Framework_MockObject_RuntimeException
     * @expectedExceptionMessage duplicates: "foo, foo"
     */
    public function testGetMockGeneratorFails()
    {
        $mock = $this->generator->getMock('StdClass', array('foo', 'foo'));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForAbstractClass
     */
    public function testGetMockForAbstractClassDoesNotFailWhenFakingInterfaces()
    {
        $mock = $this->generator->getMockForAbstractClass('Countable');
        $this->assertTrue(method_exists($mock, 'count'));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForAbstractClass
     */
    public function testGetMockForAbstractClassStubbingAbstractClass()
    {
        $mock = $this->generator->getMockForAbstractClass('AbstractMockTestClass');
        $this->assertTrue(method_exists($mock, 'doSomething'));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForAbstractClass
     */
    public function testGetMockForAbstractClassWithNonExistentMethods()
    {
        $mock = $this->generator->getMockForAbstractClass(
            'AbstractMockTestClass', array(), '',  true,
            true, true, array('nonexistentMethod')
        );

        $this->assertTrue(method_exists($mock, 'nonexistentMethod'));
        $this->assertTrue(method_exists($mock, 'doSomething'));
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForAbstractClass
     */
    public function testGetMockForAbstractClassShouldCreateStubsOnlyForAbstractMethodWhenNoMethodsWereInformed()
    {
        $mock = $this->generator->getMockForAbstractClass('AbstractMockTestClass');

        $mock->expects($this->any())
             ->method('doSomething')
             ->willReturn('testing');

        $this->assertEquals('testing', $mock->doSomething());
        $this->assertEquals(1, $mock->returnAnything());
    }

    /**
     * @dataProvider getMockForAbstractClassExpectsInvalidArgumentExceptionDataprovider
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForAbstractClass
     * @expectedException PHPUnit_Framework_Exception
     */
    public function testGetMockForAbstractClassExpectingInvalidArgumentException($className, $mockClassName)
    {
        $mock = $this->generator->getMockForAbstractClass($className, array(), $mockClassName);
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForAbstractClass
     * @expectedException PHPUnit_Framework_MockObject_RuntimeException
     */
    public function testGetMockForAbstractClassAbstractClassDoesNotExist()
    {
        $mock = $this->generator->getMockForAbstractClass('Tux');
    }

    /**
     * Dataprovider for test "testGetMockForAbstractClassExpectingInvalidArgumentException"
     */
    public static function getMockForAbstractClassExpectsInvalidArgumentExceptionDataprovider()
    {
        return array(
            'className not a string' => array(array(), ''),
            'mockClassName not a string' => array('Countable', new StdClass),
        );
    }

    /**
     * @covers PHPUnit_Framework_MockObject_Generator::getMockForTrait
     * @requires PHP 5.4.0
     */
    public function testGetMockForTraitWithNonExistentMethodsAndNonAbstractMethods()
    {
        $mock = $this->generator->getMockForTrait(
            'AbstractTrait', array(), '',  true,
            true, true, array('nonexistentMethod')
        );

        $this->assertTrue(method_exists($mock, 'nonexistentMethod'));
        $this->assertTrue(method_exists($mock, 'doSomething'));
        $this->assertTrue($mock->mockableMethod());
        $this->assertTrue($mock->anotherMockableMethod());
    }

    /**
     * @covers   PHPUnit_Framework_MockObject_Generator::getMockForTrait
     * @requires PHP 5.4.0
     */
    public function testGetMockForTraitStubbingAbstractMethod()
    {
        $mock = $this->generator->getMockForTrait('AbstractTrait');
        $this->assertTrue(method_exists($mock, 'doSomething'));
    }

    /**
     * @requires PHP 5.4.0
     */
    public function testGetMockForSingletonWithReflectionSuccess()
    {
        // Probably, this should be moved to tests/autoload.php
        require_once __DIR__ . '/_fixture/SingletonClass.php';

        $mock = $this->generator->getMock('SingletonClass', array('doSomething'), array(), '', false);
        $this->assertInstanceOf('SingletonClass', $mock);
    }

    /**
     * Same as "testGetMockForSingletonWithReflectionSuccess", but we expect
     * warning for PHP < 5.4.0 since PHPUnit will try to execute private __wakeup
     * on unserialize
     */
    public function testGetMockForSingletonWithUnserializeFail()
    {
        if (version_compare(PHP_VERSION, '5.4.0', '>=')) {
            $this->markTestSkipped('Only for PHP < 5.4.0');
        }

        $this->setExpectedException('PHPUnit_Framework_MockObject_RuntimeException');

        // Probably, this should be moved to tests/autoload.php
        require_once __DIR__ . '/_fixture/SingletonClass.php';

        $mock = $this->generator->getMock('SingletonClass', array('doSomething'), array(), '', false);
    }
}
