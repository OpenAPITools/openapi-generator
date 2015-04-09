<?php

namespace Guzzle\Tests\Cache;

use Guzzle\Cache\DoctrineCacheAdapter;
use Doctrine\Common\Cache\ArrayCache;

/**
 * @covers Guzzle\Cache\DoctrineCacheAdapter
 * @covers Guzzle\Cache\AbstractCacheAdapter
 */
class CacheAdapterTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var ArrayCache */
    private $cache;

    /** @var DoctrineCacheAdapter */
    private $adapter;

    /**
     * Prepares the environment before running a test.
     */
    protected function setUp()
    {
        parent::setUp();
        $this->cache = new ArrayCache();
        $this->adapter = new DoctrineCacheAdapter($this->cache);
    }

    /**
     * Cleans up the environment after running a test.
     */
    protected function tearDown()
    {
        $this->adapter = null;
        $this->cache = null;
        parent::tearDown();
    }

    public function testGetCacheObject()
    {
        $this->assertEquals($this->cache, $this->adapter->getCacheObject());
    }

    public function testSave()
    {
        $this->assertTrue($this->adapter->save('test', 'data', 1000));
    }

    public function testFetch()
    {
        $this->assertTrue($this->adapter->save('test', 'data', 1000));
        $this->assertEquals('data', $this->adapter->fetch('test'));
    }

    public function testContains()
    {
        $this->assertTrue($this->adapter->save('test', 'data', 1000));
        $this->assertTrue($this->adapter->contains('test'));
    }

    public function testDelete()
    {
        $this->assertTrue($this->adapter->save('test', 'data', 1000));
        $this->assertTrue($this->adapter->delete('test'));
        $this->assertFalse($this->adapter->contains('test'));
    }
}
