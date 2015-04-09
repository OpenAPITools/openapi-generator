<?php

namespace Guzzle\Tests\Iterator;

use Guzzle\Iterator\FilterIterator;

/**
 * @covers Guzzle\Iterator\FilterIterator
 */
class FilterIteratorTest extends \PHPUnit_Framework_TestCase
{
    public function testFiltersValues()
    {
        $i = new FilterIterator(new \ArrayIterator(range(0, 100)), function ($value) {
            return $value % 2;
        });

        $this->assertEquals(range(1, 99, 2), iterator_to_array($i, false));
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testValidatesCallable()
    {
        $i = new FilterIterator(new \ArrayIterator(), new \stdClass());
    }
}
