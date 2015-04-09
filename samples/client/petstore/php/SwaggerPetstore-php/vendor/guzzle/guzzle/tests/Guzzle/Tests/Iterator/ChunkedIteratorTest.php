<?php

namespace Guzzle\Tests\Iterator;

use Guzzle\Iterator\ChunkedIterator;

/**
 * @covers Guzzle\Iterator\ChunkedIterator
 */
class ChunkedIteratorTest extends \PHPUnit_Framework_TestCase
{
    public function testChunksIterator()
    {
        $chunked = new ChunkedIterator(new \ArrayIterator(range(0, 100)), 10);
        $chunks = iterator_to_array($chunked, false);
        $this->assertEquals(11, count($chunks));
        foreach ($chunks as $j => $chunk) {
            $this->assertEquals(range($j * 10, min(100, $j * 10 + 9)), $chunk);
        }
    }

    public function testChunksIteratorWithOddValues()
    {
        $chunked = new ChunkedIterator(new \ArrayIterator(array(1, 2, 3, 4, 5)), 2);
        $chunks = iterator_to_array($chunked, false);
        $this->assertEquals(3, count($chunks));
        $this->assertEquals(array(1, 2), $chunks[0]);
        $this->assertEquals(array(3, 4), $chunks[1]);
        $this->assertEquals(array(5), $chunks[2]);
    }

    public function testMustNotTerminateWithTraversable()
    {
        $traversable = simplexml_load_string('<root><foo/><foo/><foo/></root>')->foo;
        $chunked = new ChunkedIterator($traversable, 2);
        $actual = iterator_to_array($chunked, false);
        $this->assertCount(2, $actual);
    }

    public function testSizeOfZeroMakesIteratorInvalid() {
        $chunked = new ChunkedIterator(new \ArrayIterator(range(1, 5)), 0);
        $chunked->rewind();
        $this->assertFalse($chunked->valid());
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testSizeLowerZeroThrowsException() {
        new ChunkedIterator(new \ArrayIterator(range(1, 5)), -1);
    }
}
