<?php

namespace Guzzle\Tests\Iterator;

use Guzzle\Iterator\MethodProxyIterator;
use Guzzle\Iterator\ChunkedIterator;

/**
 * @covers Guzzle\Iterator\MethodProxyIterator
 */
class MethodProxyIteratorTest extends \PHPUnit_Framework_TestCase
{
    public function testProxiesMagicCallsToInnermostIterator()
    {
        $i = new \ArrayIterator();
        $proxy = new MethodProxyIterator(new MethodProxyIterator(new MethodProxyIterator($i)));
        $proxy->append('a');
        $proxy->append('b');
        $this->assertEquals(array('a', 'b'), $i->getArrayCopy());
        $this->assertEquals(array('a', 'b'), $proxy->getArrayCopy());
    }

    public function testUsesInnerIterator()
    {
        $i = new MethodProxyIterator(new ChunkedIterator(new \ArrayIterator(array(1, 2, 3, 4, 5)), 2));
        $this->assertEquals(3, count(iterator_to_array($i, false)));
    }
}
