<?php

class Framework_MockObject_Invocation_StaticTest extends PHPUnit_Framework_TestCase
{
    public function testConstructorRequiresClassAndMethodAndParameters()
    {
        new PHPUnit_Framework_MockObject_Invocation_Static('FooClass', 'FooMethod', array('an_argument'));
    }

    public function testAllowToGetClassNameSetInConstructor()
    {
        $invocation = new PHPUnit_Framework_MockObject_Invocation_Static('FooClass', 'FooMethod', array('an_argument'));

        $this->assertSame('FooClass', $invocation->className);
    }

    public function testAllowToGetMethodNameSetInConstructor()
    {
        $invocation = new PHPUnit_Framework_MockObject_Invocation_Static('FooClass', 'FooMethod', array('an_argument'));

        $this->assertSame('FooMethod', $invocation->methodName);
    }

    public function testAllowToGetMethodParametersSetInConstructor()
    {
        $expectedParameters = array(
          'foo', 5, array('a', 'b'), new StdClass, NULL, FALSE
        );

        $invocation = new PHPUnit_Framework_MockObject_Invocation_Static(
          'FooClass', 'FooMethod', $expectedParameters
        );

        $this->assertSame($expectedParameters, $invocation->parameters);
    }

    public function testConstructorAllowToSetFlagCloneObjectsInParameters()
    {
        $parameters = array(new StdClass);
        $cloneObjects = TRUE;

        $invocation = new PHPUnit_Framework_MockObject_Invocation_Static(
          'FooClass',
          'FooMethod',
          $parameters,
          $cloneObjects
        );

        $this->assertEquals($parameters, $invocation->parameters);
        $this->assertNotSame($parameters, $invocation->parameters);
    }
}
