<?php

namespace Guzzle\Tests\Inflection;

use Guzzle\Inflection\MemoizingInflector;
use Guzzle\Inflection\Inflector;

/**
 * @covers Guzzle\Inflection\MemoizingInflector
 */
class MemoizingInflectorTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testUsesCache()
    {
        $mock = $this->getMock('Guzzle\Inflection\Inflector', array('snake', 'camel'));
        $mock->expects($this->once())->method('snake')->will($this->returnValue('foo_bar'));
        $mock->expects($this->once())->method('camel')->will($this->returnValue('FooBar'));

        $inflector = new MemoizingInflector($mock);
        $this->assertEquals('foo_bar', $inflector->snake('FooBar'));
        $this->assertEquals('foo_bar', $inflector->snake('FooBar'));
        $this->assertEquals('FooBar', $inflector->camel('foo_bar'));
        $this->assertEquals('FooBar', $inflector->camel('foo_bar'));
    }

    public function testProtectsAgainstCacheOverflow()
    {
        $inflector = new MemoizingInflector(new Inflector(), 10);
        for ($i = 1; $i < 11; $i++) {
            $inflector->camel('foo_' . $i);
            $inflector->snake('Foo' . $i);
        }

        $cache = $this->readAttribute($inflector, 'cache');
        $this->assertEquals(10, count($cache['snake']));
        $this->assertEquals(10, count($cache['camel']));

        $inflector->camel('baz!');
        $inflector->snake('baz!');

        // Now ensure that 20% of the cache was removed (2), then the item was added
        $cache = $this->readAttribute($inflector, 'cache');
        $this->assertEquals(9, count($cache['snake']));
        $this->assertEquals(9, count($cache['camel']));
    }
}
