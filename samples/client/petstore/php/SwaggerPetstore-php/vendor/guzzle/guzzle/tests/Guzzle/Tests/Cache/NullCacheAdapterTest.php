<?php

namespace Guzzle\Tests\Common\Cache;

use Guzzle\Cache\NullCacheAdapter;

/**
 * @covers Guzzle\Cache\NullCacheAdapter
 */
class NullCacheAdapterTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testNullCacheAdapter()
    {
        $c = new NullCacheAdapter();
        $this->assertEquals(false, $c->contains('foo'));
        $this->assertEquals(true, $c->delete('foo'));
        $this->assertEquals(false, $c->fetch('foo'));
        $this->assertEquals(true, $c->save('foo', 'bar'));
    }
}
