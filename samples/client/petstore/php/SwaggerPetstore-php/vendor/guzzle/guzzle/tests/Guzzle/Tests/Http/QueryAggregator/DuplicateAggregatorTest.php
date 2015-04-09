<?php

namespace Guzzle\Tests\Http;

use Guzzle\Http\QueryString;
use Guzzle\Http\QueryAggregator\DuplicateAggregator as Ag;

class DuplicateAggregatorTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testAggregates()
    {
        $query = new QueryString();
        $a = new Ag();
        $key = 'facet 1';
        $value = array('size a', 'width b');
        $result = $a->aggregate($key, $value, $query);
        $this->assertEquals(array('facet%201' => array('size%20a', 'width%20b')), $result);
    }

    public function testEncodes()
    {
        $query = new QueryString();
        $query->useUrlEncoding(false);
        $a = new Ag();
        $key = 'facet 1';
        $value = array('size a', 'width b');
        $result = $a->aggregate($key, $value, $query);
        $this->assertEquals(array('facet 1' => array('size a', 'width b')), $result);
    }
}
