<?php
/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState enabled
 */
class Issue1335Test extends PHPUnit_Framework_TestCase
{
    public function testGlobalString()
    {
        $this->assertEquals("Hello", $GLOBALS['globalString']);
    }

    public function testGlobalIntTruthy()
    {
        $this->assertEquals(1, $GLOBALS['globalIntTruthy']);
    }

    public function testGlobalIntFalsey()
    {
        $this->assertEquals(0, $GLOBALS['globalIntFalsey']);
    }

    public function testGlobalFloat()
    {
        $this->assertEquals(1.123, $GLOBALS['globalFloat']);
    }

    public function testGlobalBoolTrue()
    {
        $this->assertEquals(true, $GLOBALS['globalBoolTrue']);
    }

    public function testGlobalBoolFalse()
    {
        $this->assertEquals(false, $GLOBALS['globalBoolFalse']);
    }

    public function testGlobalNull()
    {
        $this->assertEquals(null, $GLOBALS['globalNull']);
    }

    public function testGlobalArray()
    {
        $this->assertEquals(array("foo"), $GLOBALS['globalArray']);
    }

    public function testGlobalNestedArray()
    {
        $this->assertEquals(array(array("foo")), $GLOBALS['globalNestedArray']);
    }

    public function testGlobalObject()
    {
        $this->assertEquals((object)array("foo"=>"bar"), $GLOBALS['globalObject']);
    }

    public function testGlobalObjectWithBackSlashString()
    {
        $this->assertEquals((object)array("foo"=>"back\\slash"), $GLOBALS['globalObjectWithBackSlashString']);
    }

    public function testGlobalObjectWithDoubleBackSlashString()
    {
        $this->assertEquals((object)array("foo"=>"back\\\\slash"), $GLOBALS['globalObjectWithDoubleBackSlashString']);
    }
}
