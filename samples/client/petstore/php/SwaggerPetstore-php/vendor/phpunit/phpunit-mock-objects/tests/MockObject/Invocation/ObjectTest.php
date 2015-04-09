<?php

class Framework_MockObject_Invocation_ObjectTest extends PHPUnit_Framework_TestCase
{
    public function testConstructorRequiresClassAndMethodAndParametersAndObject()
    {
        new PHPUnit_Framework_MockObject_Invocation_Object(
            'FooClass',
            'FooMethod',
            array('an_argument'),
            new StdClass);
    }

    public function testAllowToGetClassNameSetInConstructor()
    {
        $invocation = new PHPUnit_Framework_MockObject_Invocation_Object(
            'FooClass',
            'FooMethod',
            array('an_argument'),
            new StdClass);

        $this->assertSame('FooClass', $invocation->className);
    }

    public function testAllowToGetMethodNameSetInConstructor()
    {
        $invocation = new PHPUnit_Framework_MockObject_Invocation_Object(
            'FooClass',
            'FooMethod',
            array('an_argument'),
            new StdClass);

        $this->assertSame('FooMethod', $invocation->methodName);
    }

    public function testAllowToGetObjectSetInConstructor()
    {
        $expectedObject = new StdClass;

        $invocation = new PHPUnit_Framework_MockObject_Invocation_Object(
            'FooClass',
            'FooMethod',
            array('an_argument'),
            $expectedObject);

        $this->assertSame($expectedObject, $invocation->object);
    }

    public function testAllowToGetMethodParametersSetInConstructor()
    {
        $expectedParameters = array(
          'foo', 5, array('a', 'b'), new StdClass, NULL, FALSE
        );

        $invocation = new PHPUnit_Framework_MockObject_Invocation_Object(
          'FooClass',
          'FooMethod',
          $expectedParameters,
          new StdClass
        );

        $this->assertSame($expectedParameters, $invocation->parameters);
    }

    public function testConstructorAllowToSetFlagCloneObjectsInParameters()
    {
        $parameters   = array(new StdClass);
        $cloneObjects = TRUE;

        $invocation = new PHPUnit_Framework_MockObject_Invocation_Object(
          'FooClass',
          'FooMethod',
          $parameters,
          new StdClass,
          $cloneObjects
        );

        $this->assertEquals($parameters, $invocation->parameters);
        $this->assertNotSame($parameters, $invocation->parameters);
    }
}
