<?php

namespace Guzzle\Tests\Inflection;

use Guzzle\Inflection\PreComputedInflector;

/**
 * @covers Guzzle\Inflection\PreComputedInflector
 */
class PreComputedInflectorTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testUsesPreComputedHash()
    {
        $mock = $this->getMock('Guzzle\Inflection\Inflector', array('snake', 'camel'));
        $mock->expects($this->once())->method('snake')->with('Test')->will($this->returnValue('test'));
        $mock->expects($this->once())->method('camel')->with('Test')->will($this->returnValue('Test'));
        $inflector = new PreComputedInflector($mock, array('FooBar' => 'foo_bar'), array('foo_bar' => 'FooBar'));
        $this->assertEquals('FooBar', $inflector->camel('foo_bar'));
        $this->assertEquals('foo_bar', $inflector->snake('FooBar'));
        $this->assertEquals('Test', $inflector->camel('Test'));
        $this->assertEquals('test', $inflector->snake('Test'));
    }

    public function testMirrorsPrecomputedValues()
    {
        $mock = $this->getMock('Guzzle\Inflection\Inflector', array('snake', 'camel'));
        $mock->expects($this->never())->method('snake');
        $mock->expects($this->never())->method('camel');
        $inflector = new PreComputedInflector($mock, array('Zeep' => 'zeep'), array(), true);
        $this->assertEquals('Zeep', $inflector->camel('zeep'));
        $this->assertEquals('zeep', $inflector->snake('Zeep'));
    }

    public function testMirrorsPrecomputedValuesByMerging()
    {
        $mock = $this->getMock('Guzzle\Inflection\Inflector', array('snake', 'camel'));
        $mock->expects($this->never())->method('snake');
        $mock->expects($this->never())->method('camel');
        $inflector = new PreComputedInflector($mock, array('Zeep' => 'zeep'), array('foo' => 'Foo'), true);
        $this->assertEquals('Zeep', $inflector->camel('zeep'));
        $this->assertEquals('zeep', $inflector->snake('Zeep'));
        $this->assertEquals('Foo', $inflector->camel('foo'));
        $this->assertEquals('foo', $inflector->snake('Foo'));
    }
}
