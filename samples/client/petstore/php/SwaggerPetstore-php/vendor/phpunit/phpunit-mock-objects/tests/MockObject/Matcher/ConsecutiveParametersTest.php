<?php
class Framework_MockObject_Matcher_ConsecutiveParametersTest extends PHPUnit_Framework_TestCase
{
    public function testIntegration()
    {
        $mock = $this->getMock('stdClass', array('foo'));
        $mock
            ->expects($this->any())
            ->method('foo')
            ->withConsecutive(
                array('bar'),
                array(21, 42)
            );
        $mock->foo('bar');
        $mock->foo(21, 42);
    }

    public function testIntegrationWithLessAssertionsThenMethodCalls()
    {
        $mock = $this->getMock('stdClass', array('foo'));
        $mock
            ->expects($this->any())
            ->method('foo')
            ->withConsecutive(
              array('bar')
            );
        $mock->foo('bar');
        $mock->foo(21, 42);
    }

    public function testIntegrationExpectingException()
    {
        $mock = $this->getMock('stdClass', array('foo'));
        $mock
            ->expects($this->any())
            ->method('foo')
            ->withConsecutive(
                array('bar'),
                array(21, 42)
            );
        $mock->foo('bar');
        $this->setExpectedException('PHPUnit_Framework_ExpectationFailedException');
        $mock->foo('invalid');
    }
}